# DataFrames vs Circular Buffers: Processing Large Datasets Efficiently in Pharo

When processing large datasets in Pharo, developers typically reach for DataFrames and for good reason. They provide a powerful way to manipulate and analyze data, similar to how you would in Python or R. However, there's a hidden problem that many developers overlook: DataFrames can be inefficient and even detrimental to performance when dealing with large files.

In this article, we'll explore why DataFrames can hurt performance, how circular buffers can be a better alternative, and provide practical examples to illustrate the differences. We'll also cover how to generate test data and measure performance effectively.

## The Hidden Problem: Why DataFrames Can Hurt Performance
Let me show you a common scenario that many developers face when working with DataFrames. Imagine you have a CSV file with stock prices, and you want to calculate the average price. Here's how you might do it using DataFrames:

```smalltalk
stockDataFile := 'stock_data.csv'.

"Load entire CSV file into DataFrame"
stockData := DataFrame readFromCsv: stockDataFile asFileReference.
priceColumn := stockData column: 'Price'.
averagePrice := priceColumn average.
Transcript show: 'Total Average Price: ', averagePrice asString; cr.
```

This works perfectly for small files. But here's what happens when your CSV file grows from 1MB to 100MB to 1GB:

**The Problems:**
- **Memory Overhead**: DataFrames load the entire file into memory, which can be 10x larger than the file size itself. For a 1GB file, you might end up using 10GB of RAM.
- **Garbage Collection**: As the DataFrame grows, garbage collection kicks in frequently, slowing down your program. This is because DataFrames create many temporary objects that need to be cleaned up.
- **Performance**: Calculating the average involves iterating through potentially millions of rows, which can take a long time. The more data you have, the slower it gets.
- **Crashes**: If the file is larger than your available RAM, you get "Out of Memory" errors, causing your program to crash.


Think of it like this: you want to know the average height of people in a room, so you ask everyone to stand in line and write down everyone's details in a notebook. That's what DataFrames do - they store everything, even when you only need one number.


## Moving Averages: A Practical Example
Let's take this a step further and calculate moving averages, which is a common task in financial applications. We'll compare the DataFrame approach with the circular buffer approach.

### Generating Test Data
Let's generate some realistic test data to see how these two approaches perform in practice. We'll create a CSV file with multiple columns of stock market data:
```smalltalk
| stockDataFile rowCount windowSize |

stockDataFile := 'moving_avg_test.csv'.
rowCount := 500000.
windowSize := 100.

"Clean up any old files"
stockDataFile asFileReference exists ifTrue: [
    stockDataFile asFileReference delete
].

Transcript show: 'BENCHMARK: Moving Average - DataFrame vs Streaming Buffer'; cr.

"Generate simple test data"
Transcript show: 'Generating test data...'; cr.
stockDataFile asFileReference writeStreamDo: [ :stream |
    | currentPrice |
    currentPrice := 100.0.
    stream nextPutAll: 'S.No.,Price,Low,High'; cr.
    
    1 to: rowCount do: [ :day |
        | priceChange newPrice lowPrice highPrice |
        priceChange := (Random new next - 0.5) * 2.0.  "±$1 change"
        newPrice := currentPrice + priceChange.
        
        "Generate realistic Low and High values around the price"
        lowPrice := newPrice - (Random new next * 2.0).  "Low is below price"
        highPrice := newPrice + (Random new next * 2.0). "High is above price"
        
        stream nextPutAll: day asString, ',',
                          (newPrice roundTo: 0.01) asString, ',',
                          (lowPrice roundTo: 0.01) asString, ',',
                          (highPrice roundTo: 0.01) asString; cr.
        
        currentPrice := newPrice.
    ].
].
Transcript show: 'Generated file: ', (stockDataFile asFileReference size / 1024 / 1024) rounded asString, ' MB'; cr; cr.
```
This code generates a CSV file with 500,000 rows of realistic stock market data, including serial numbers, prices, daily lows, and highs. Each price is a random fluctuation around the previous day's price.
## Performance Testing: The Numbers Tell the Story
Now let's compare the performance of the DataFrame approach with the circular buffer approach for calculating moving averages.

### Test Setup
```smalltalk
"Test DataFrame approach"
Transcript show: 'Testing DataFrame approach...'; cr.
3 timesRepeat: [ Smalltalk garbageCollect ].
[
    | startTime endTime memoryBefore memoryAfter gcBefore gcAfter gcTimeBefore gcTimeAfter 
		stockData priceColumn movingAverages |
    
	 memoryBefore := Smalltalk vm parameterAt: 3.
    gcBefore := (Smalltalk vm parameterAt: 7) + (Smalltalk vm parameterAt: 9).
    gcTimeBefore := (Smalltalk vm parameterAt: 8) + (Smalltalk vm parameterAt: 10).
    startTime := Time millisecondClockValue.
    
    "Load entire dataset"
    stockData := DataFrame readFromCsv: stockDataFile asFileReference.
    priceColumn := stockData column: 'Price'.
    
    movingAverages := OrderedCollection new.
    
    windowSize to: priceColumn size do: [ :currentDay |
        | windowSum |
        windowSum := 0.
        
        (currentDay - windowSize + 1) to: currentDay do: [ :j |
            windowSum := windowSum + (priceColumn at: j).
        ].
        
        movingAverages add: (windowSum / windowSize).
    ].
    
    endTime := Time millisecondClockValue.
    memoryAfter := Smalltalk vm parameterAt: 3.
    gcAfter := (Smalltalk vm parameterAt: 7) + (Smalltalk vm parameterAt: 9).
    gcTimeAfter := (Smalltalk vm parameterAt: 8) + (Smalltalk vm parameterAt: 10).
    Transcript show: 'DataFrame Test Results:'; cr.
    Transcript show: '   Time: ', (endTime - startTime) asString, ' ms'; cr.
    Transcript show: '   Memory: ', ((memoryAfter - memoryBefore) / 1024 / 1024) rounded asString, ' MB'; cr.
    Transcript show: '   GC Events: ', (gcAfter - gcBefore) asString; cr.
	 Transcript show: '   GC Time: ', (gcTimeAfter - gcTimeBefore) asString, ' ms'; cr.
    Transcript show: '   Moving Averages: ', movingAverages size asString; cr.
    Transcript show: '   Final MA: $', (movingAverages last roundTo: 0.01) asString; cr; cr.
    
    "Cleanup to be fair"
    stockData := nil.
    priceColumn := nil.
    movingAverages := nil.
] value.

"Test Circular Buffer approach"
Transcript show: 'Testing Buffer approach...'; cr.

3 timesRepeat: [ Smalltalk garbageCollect ].
[
    | startTime endTime memoryBefore memoryAfter gcBefore gcAfter gcTimeBefore gcTimeAfter
      priceBuffer movingAverages processedCount |
    
    memoryBefore := Smalltalk vm parameterAt: 3.
    gcBefore := (Smalltalk vm parameterAt: 7) + (Smalltalk vm parameterAt: 9).
    gcTimeBefore := (Smalltalk vm parameterAt: 8) + (Smalltalk vm parameterAt: 10).
    startTime := Time millisecondClockValue.
    
    priceBuffer := CTFIFOBuffer new: windowSize.
    movingAverages := OrderedCollection new.
    processedCount := 0.
    
    stockDataFile asFileReference readStreamDo: [ :fileStream |
        | line |
        fileStream atEnd ifFalse: [ fileStream nextLine ].
        
        [ fileStream atEnd ] whileFalse: [
            line := fileStream nextLine.
            
            line ifNotEmpty: [
                | csvParts price |
                csvParts := line splitOn: ','.
                price := (csvParts at: 2) asNumber.  "Price is 2nd column"
                
                priceBuffer push: price.
                processedCount := processedCount + 1.
                
                "Calculate moving average when buffer is full"
                priceBuffer isFull ifTrue: [
                    | bufferSum movingAvg |
                    bufferSum := 0.
                    priceBuffer do: [ :p | bufferSum := bufferSum + p ].
                    movingAvg := bufferSum / priceBuffer size.
                    movingAverages add: movingAvg.
                ].
            ].
        ].
    ].
    
    endTime := Time millisecondClockValue.
    memoryAfter := Smalltalk vm parameterAt: 3.
    gcAfter := (Smalltalk vm parameterAt: 7) + (Smalltalk vm parameterAt: 9).
    gcTimeAfter := (Smalltalk vm parameterAt: 8) + (Smalltalk vm parameterAt: 10).
    
    Transcript show: 'Buffer Results:'; cr.
    Transcript show: '   Time: ', (endTime - startTime) asString, ' ms'; cr.
    Transcript show: '   Memory: ', ((memoryAfter - memoryBefore) / 1024 / 1024) rounded asString, ' MB'; cr.
    Transcript show: '   GC Events: ', (gcAfter - gcBefore) asString; cr.
	 Transcript show: '   GC Time: ', (gcTimeAfter - gcTimeBefore) asString, ' ms'; cr.
    Transcript show: '   Moving Averages: ', movingAverages size asString; cr.
    Transcript show: '   Final MA: $', (movingAverages last roundTo: 0.01) asString; cr; cr.
] value.


"Cleanup..."
stockDataFile asFileReference delete.
Transcript show: 'Tests Done!'; cr.
```

### Benchmark Results
Here are the results from running this benchmark on a 500,000-row dataset (approximately 15MB file):
| Metric | DataFrame | Circular Buffer | Improvement |
|--------|-----------|-----------------|-------------|
| **Execution Time** | ~15,100 ms | ~2,100 ms | **7.2x faster** |
| **Memory Usage** | ~128 MB | ~16 MB | **8x less memory** |
| **GC Events** | ~870 | ~52 | **94% fewer** |
| **GC Time** | ~3,500 ms | ~3 ms | **1200x less GC overhead** |
| **Results Generated** | 499,901 | 499,901 | Identical accuracy |

*Note: Results may vary based on your hardware, Pharo version, and system load*

**Key Insights:**
- Circular buffers processed the same data **7.2x faster**
- Used **8x less memory** despite processing the same amount of data
- Had **94% fewer garbage collection events**, leading to smoother performance
- Spent virtually no time on memory cleanup (3ms vs 3.5+ seconds)
- Produced identical results, proving accuracy isn't compromised

## When to Use Each Approach

### Use DataFrames When:
- Your entire dataset comfortably fits in memory (under 100MB typically)
- You need complex operations like joins, group-by, or statistical functions
- You're doing one-time analysis where you explore data interactively
- You need to sort, filter, or query data in complex ways

### Use Circular Buffers When:
- Processing large files (over 100MB)
- Computing simple statistics (averages, sums, counts)
- Building real-time systems that process continuous data streams
- Working with memory-limited environments
- Processing data bigger than your available RAM

## Try It Yourself

1. Generate some test data using the code above
2. Run both approaches on files of different sizes
3. Watch the memory usage and timing differences
4. See how circular buffers handle files bigger than your RAM

You'll be surprised at how much faster your programs can run when you choose the right data structure for the job.

## Summary
In this article, we explored the limitations of DataFrames when processing large datasets in Pharo and introduced circular buffers as a more efficient alternative. We demonstrated how circular buffers can handle large files without running out of memory, while also being significantly faster for simple computations like averages and moving averages.

We also provided practical examples of generating test data and measuring performance for both approaches. The key takeaway is that sometimes the simplest solution is the fastest solution, especially when dealing with large datasets.


Want to explore more? Check out the [Containers-Buffer repository](https://github.com/pharo-containers/Containers-Buffer) to see the complete implementation and examples.