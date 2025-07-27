# DataFrames vs Circular Buffers: Processing Large Datasets Efficiently in Pharo

When processing large datasets in Pharo, developers typically reach for DataFrames and for good reason. They provide a powerful way to manipulate and analyze data, similar to proven solutions like [Pandas in Python](https://pandas.pydata.org/), [data.frame in R](https://www.r-project.org/), or [Pharo's DataFrame implementation](https://github.com/PolyMathOrg/DataFrame). DataFrames excel at complex operations like joins, grouping, statistical analysis, and data exploration. However, not all data processing tasks require loading an entire dataset into memory. For certain problems, especially those involving massive files and streaming computations, a more specialized tool can offer significant performance benefits. This is where a circular buffer shines.

In this article, we'll explore when each approach shines, provide practical examples with realistic large datasets, and demonstrate how choosing the right data structure can dramatically improve performance for specific use cases.

## Understanding the Trade-offs

Let's consider a common task: Calculating the average price from a CSV file of stock data. With a DataFrame, the approach is straightforward. Here's how you might do it using DataFrames:

```smalltalk
stockDataFile := 'stock_data.csv'.

"Load entire CSV file into DataFrame"
stockData := DataFrame readFromCsv: stockDataFile asFileReference.
priceColumn := stockData column: 'Price'.
averagePrice := priceColumn average.
Transcript show: 'Total Average Price: ', averagePrice asString; cr.
```

This works beautifully for moderately sized files. But what happens when your dataset isn't just a few megabytes, but grows to 20GB, 100GB, or even larger? In these real-world scenarios, loading the entire file into memory can lead to significant challenges:

**The Problems:**
- **Garbage Collection**: As the DataFrame grows, garbage collection kicks in frequently, slowing down your program. This is because DataFrames create many temporary objects that need to be cleaned up.
- **Performance**: Calculating the average involves iterating through potentially millions of rows, which can take a long time. The more data you have, the slower it gets.
- **Extreme Memory Pressure**: If a dataset is larger than the available RAM, the operating system starts using the hard disk as virtual memory. This process will make your program very slow, as disk access is orders of magnitude slower than RAM access.

Think of it this way: to find the average height of people in a large crowd, you don't need a detailed notebook with everyone's name, age, and address. You just need to sum their heights and divide by the count. The circular buffer approach is like having a simple calculator and notepad, it only keeps the information essential for the immediate task.

## Moving Averages: A Practical Example
Let's take this a step further and calculate moving averages, which is a common task in financial applications. We'll compare the DataFrame approach with the circular buffer approach.

### Generating Test Data
Let's generate some realistic test data to see how these two approaches perform in practice. We'll create a CSV file with multiple columns of stock market data:
```smalltalk
| stockDataFile rowCount windowSize |

stockDataFile := 'moving_avg_test.csv'.
rowCount := 8000000.
windowSize := 100.

"Clean up any old files"
stockDataFile asFileReference exists ifTrue: [
    stockDataFile asFileReference delete
].

Transcript show: 'BENCHMARK: Moving Average - DataFrame vs Streaming Buffer'; cr.

Transcript show: 'Generating test data...'; cr.
stockDataFile asFileReference writeStreamDo: [ :stream |
    | currentPrice |
    currentPrice := 150.0.

    stream nextPutAll: 'S.No.,Symbol,Price,Volume,Low,High,OpenInterest'; cr.

    1 to: rowCount do: [ :day |
        | priceChange newPrice lowPrice highPrice volume openInterest |
        priceChange := (Random new next - 0.5) * 2.0.
        newPrice := (currentPrice + priceChange) max: 1.0.
        lowPrice := newPrice - (Random new next * 2.0).
        highPrice := newPrice + (Random new next * 2.0).
        
        volume := 50000 + (1950000 atRandom) rounded.
        openInterest := 1000 + (49000 atRandom) rounded.

        stream
            nextPutAll: day asString; nextPut: $,;
            nextPutAll: 'PHARO-STOCK'; nextPut: $,;
            nextPutAll: (newPrice roundTo: 0.01) asString; nextPut: $,;
            nextPutAll: volume asString; nextPut: $,;
            nextPutAll: (lowPrice roundTo: 0.01) asString; nextPut: $,;
            nextPutAll: (highPrice roundTo: 0.01) asString; nextPut: $,;
            nextPutAll: openInterest asString; cr.

        currentPrice := newPrice.
    ].
].
Transcript show: 'Generated file: ', (stockDataFile asFileReference size / 1024 / 1024) rounded asString, ' MB'; cr; cr.
```

This code generates a CSV file with 8,000,000 rows of stock data, simulating daily price changes. Each row contains columns for the stock symbol, price, volume, low, high, and open interest.

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
    
	 memoryBefore := Smalltalk vm parameterAt: 34.
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
    memoryAfter := Smalltalk vm parameterAt: 34.
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
    | startTime endTime memoryBefore memoryAfter gcBefore gcAfter gcTimeBefore gcTimeAfter priceBuffer movingAverages |

    memoryBefore := Smalltalk vm parameterAt: 34.
    gcBefore := (Smalltalk vm parameterAt: 7) + (Smalltalk vm parameterAt: 9).
    gcTimeBefore := (Smalltalk vm parameterAt: 8) + (Smalltalk vm parameterAt: 10).
    startTime := Time millisecondClockValue.
    
    priceBuffer := CTFIFOBuffer new: windowSize.
    movingAverages := OrderedCollection new.

    stockDataFile asFileReference readStreamDo: [ :fileStream |
        fileStream atEnd ifFalse: [ fileStream nextLine ].
        
        [ fileStream atEnd ] whileFalse: [
            | line |
            line := fileStream nextLine.
            
            line ifNotEmpty: [
                | csvParts price |
                csvParts := line splitOn: ','.
                price := (csvParts at: 3) asNumber.
                priceBuffer push: price.

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
    memoryAfter := Smalltalk vm parameterAt: 34.
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

Here are the results from running this benchmark on a 8,000,000 row dataset (approximately 450MB file):

| Metric | DataFrame | Circular Buffer | Improvement |
|--------|-----------|-----------------|-------------|
| **Execution Time** | ~968,804 ms | ~46,258 ms | **21x faster** |
| **Memory Usage** | ~222,730 MB | ~14,148 MB | **16x less memory** |
| **GC Events** | ~14,943 | ~944 | **94% fewer** |
| **GC Time** | ~826,382 ms | ~405 ms | **2,040x less GC overhead** |
| **Results Generated** | 7,999,901 | 7,999,901 | Identical accuracy |

*Note: Results may vary based on your hardware, Pharo version, and system load*

**Key Insights:**
- Circular buffers processed the same data **21x faster**
- Used **10.6x less memory** despite processing the same amount of data
- Had **94% fewer garbage collection events**, leading to smoother performance
- Spent virtually no time on memory cleanup (405ms vs 826+ seconds)
- Produced identical results, proving accuracy isn't compromised

## When to Use Each Approach
Choosing between a DataFrame and a Circular Buffer isn't about which is "better", it's about picking the right tool for the job.

### Use DataFrames When:
- Your entire dataset comfortably fits in memory
- You need complex operations like joins, group-by, or statistical functions
- You're doing one-time analysis where you explore data interactively

### Use Circular Buffers When:
- Processing data bigger than your available RAM
- Working with memory-limited environments
- You only need to look at a small, sliding "window" of data at a time (like for a moving average)

## Try It Yourself

1. Generate test data using the code above
2. Run both approaches & compare the results

You'll be surprised at how much faster your programs can run when you choose the right data structure for the job.

## Summary
DataFrames are, and will remain, an essential, powerful, and correct choice for a wide range of data analysis tasks. Their flexibility for interactive exploration is unmatched when working with datasets that fit in memory. 

However, when dataset is massive & we need only simple manipulations like calculating moving averages which we could do with a sliding window without loading everything into memory, a circular buffer can provide significant performance benefits. The key takeaway is to understand your data processing needs and choose the right tool for the job.

Want to explore more? Check out the [Containers-Buffer repository](https://github.com/pharo-containers/Containers-Buffer) to see the complete implementation and examples.