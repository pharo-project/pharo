filein/out
fileOutOn: aFileStream
	"Write in aFileStream like:
	3 run, 2 passes, 0 expected failures, 1 failures, 0 errors, 0 unexpected passes
	Failures:
	SHParserST80Test>>#testNumbers
	
	Errors:"	
	| printer |
	printer := [:title :testCases | 
				aFileStream cr; nextPutAll: title; cr.
				testCases do: [:aTestCase| 
								aTestCase printOn: aFileStream.
								aFileStream cr]].
				
	self printOn: aFileStream.	
	printer value: 'Failures:' value: self failures.
	printer value: 'Errors:' value: self errors.