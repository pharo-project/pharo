tallying
testTallySends
	"
	self debug: #testTallySends
	"
	
	| tally thisMethod allProcesses numberOfProcesses processUsedByTally tallyForPrintString tallyForPrintStringBase |
	allProcesses := Process allInstances.
	numberOfProcesses := allProcesses size.

	tally := MessageTally 
				tallySendsTo: nil 
				inBlock:  [ 3.14159 printString ] 
				showTree: true 
				closeAfter: false 
				openResultWindow: false.
	
	"tally is for this test method"
	thisMethod := self class >> #testTallySends.
	self assert: (tally hash = thisMethod hash).
	self assert: (tally method == thisMethod).
	self assert: (tally theClass == self class).


	processUsedByTally := tally process.
	self deny: (allProcesses includes: processUsedByTally).

	"Below is specialized for the execution ' 3.14159 printString '. Any method change in the execution flow of printString will turn this test to yellow"
	self assert: (tally tally >= 50).
	"The tally is not close, therefore the tree may be crawled over"
	self assert: (tally receivers isArray).
	self assert: (tally receivers size = 2).
	
	"--------"
	tallyForPrintString := tally receivers second.
	"Since 3.14159 is a float"
	self assert: (tallyForPrintString theClass includesBehavior: Float).
	"the executed method is Number>>printString"
	self assert: (tallyForPrintString method == ( Number>>#printString)).
	self assert: (tallyForPrintString tally >= 50).

	"--------"
	tallyForPrintStringBase := tallyForPrintString receivers first.
	"The receiver is still a Float"
	self assert: (tallyForPrintString theClass includesBehavior: Float).
	"the executed method is Number>>printStringBase: this time"
	self assert: (tallyForPrintStringBase method == ( Number>>#printStringBase:)).
	self assert: (tallyForPrintStringBase tally >= 50).
	
	"The method printStringBase: calls two methods:
	   SequenceableCollection class >> streamContents: and Float >> printOn:base:"
	
	self assert: (tallyForPrintStringBase receivers size = 1).
	
	"streamContents: is been tallied 13 times and printOn:base: 59 times"
	self assert: (tallyForPrintStringBase receivers size = 1).	
	self assert: ((tallyForPrintStringBase receivers first tally) >= 128 ).	


	"We close to explicitely release reference of the process, the class and methods"
	tally close.
	
	
