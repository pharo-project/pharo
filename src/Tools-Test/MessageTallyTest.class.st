"
SUnit tests for MessageTally
"
Class {
	#name : #MessageTallyTest,
	#superclass : #TestCase,
	#category : #'Tools-Test-Profilers'
}

{ #category : #tallying }
MessageTallyTest >> testClosedTally [

	| tally |
	tally := MessageTally
				tallySendsTo: nil 
				inBlock:  [ 3.14159 printString ] 
				showTree: true 
				closeAfter: true 
				openResultWindow: false.
				
	"The tally is closed, therefore nothing may be accessed"
	self assert: tally receivers isNil
]

{ #category : #basic }
MessageTallyTest >> testInstantiation [

	| messageTally |
	messageTally := MessageTally new.
	self assert: messageTally time equals: 0.
	self assert: messageTally tally equals: 0
]

{ #category : #sampling }
MessageTallyTest >> testSampling1 [

	| tally |
	tally := MessageTally
				spyOn: [ 1000  timesRepeat: [ 3.14159 printString  ] ]  
				reportOtherProcesses: false 
				cutoff: 1 
				openResultWindow: false
				closeAfter: false.

	"No process is accessible from the tally, the reason is that using the sampling method execute the block in the current process"
	self assert: tally process isNil


]

{ #category : #tallying }
MessageTallyTest >> testTallySends [
	| tally thisMethod allProcesses processUsedByTally tallyForPrintString tallyForPrintStringBase |
	allProcesses := Process allInstances.

	tally := MessageTally
		tallySendsTo: nil
		inBlock: [ 3.14159s printString ]
		showTree: true
		closeAfter: false
		openResultWindow: false.

	"tally is for this test method"
	thisMethod := self class >> #testTallySends.
	self assert: tally hash equals: thisMethod hash.
	self assert: tally method identicalTo: thisMethod.
	self assert: tally theClass identicalTo: self class.


	processUsedByTally := tally process.
	self deny: (allProcesses includes: processUsedByTally).

	"Below is specialized for the execution ' 3.14159 printString '. Any method change in the execution flow of printString will turn this test to yellow"
	self assert: tally tally >= 50.
	"The tally is not close, therefore the tree may be crawled over"
	self assert: tally receivers isArray.
	self assert: tally receivers size equals: 2.

	"--------"
	tallyForPrintString := tally receivers second.
	"Since 3.14159s is a ScaledDecimal"
	self assert: (tallyForPrintString theClass includesBehavior: ScaledDecimal).
	"the executed method is Number>>printString"
	self assert: tallyForPrintString method identicalTo: Number >> #printString.
	self assert: tallyForPrintString tally >= 50.

	"--------"
	tallyForPrintStringBase := tallyForPrintString receivers first.
	"The receiver is still a ScaledDecimal"
	self assert: (tallyForPrintString theClass includesBehavior: ScaledDecimal).
	"the executed method is Number>>printStringBase: this time"
	self assert: tallyForPrintStringBase method identicalTo: Number >> #printStringBase:.
	self assert: tallyForPrintStringBase tally >= 50.

	"The method printStringBase: calls two methods:
	   SequenceableCollection class >> streamContents: and ScaledDecimal >> printOn:base:"

	self assert: tallyForPrintStringBase receivers size equals: 1.
	self assert: tallyForPrintStringBase receivers first tally >= 50.

	"We close to explicitly release reference of the process, the class and methods"
	tally close
]

{ #category : #tallying }
MessageTallyTest >> testTallyShouldNotContainsAReturn [

	"The block passed to MessageTally should not have a ^ "
	
	self should: [ MessageTally
						tallySendsTo: nil 
						inBlock:  [ ^ 3.14159 printString ] 
						showTree: true 
						closeAfter: false 
						openResultWindow: false ] 
		 raise: Error
	
]
