tests tally
testFullPrintOn
	| stream |
	stream := WriteStream on: String new.
	tally 
		fullPrintOn: stream tallyExact: false orThreshold: 0 time: 10.
	
	self assert: stream contents =  '**Tree**

**Leaves**
'