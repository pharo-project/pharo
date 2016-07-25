tests tally
testTallyTreePrintWithTab
	| stream |
	stream := WriteStream on: String new.
	tally 
		treePrintOn: stream 
		tabs: #(' ')
		thisTab: nil 
		total: 50 
		totalTime: 100 
		tallyExact: true 
		orThreshold: 1.
	
	
	self assert: stream contents = ' 0 AndreasSystemProfilerTest  testPrintingTally
'