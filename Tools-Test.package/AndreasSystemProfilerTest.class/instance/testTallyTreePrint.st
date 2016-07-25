tests tally
testTallyTreePrint
	| stream |
	stream := WriteStream on: String new.
	tally 
		treePrintOn: stream 
		tabs: #() 
		thisTab: nil 
		total: 50 
		totalTime: 100 
		tallyExact: true 
		orThreshold: 1.
	
	"Nothing is printed since there is no tab"
	self assert: stream contents isEmpty