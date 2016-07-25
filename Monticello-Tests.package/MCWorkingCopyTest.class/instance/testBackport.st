tests
testBackport
	| inst base final backported ancestry |
	inst := self mockInstanceA.
	base := self snapshot.
	
	self assert: inst one = 1.
	
	self change: #one toReturn: 2.
	self change: #two toReturn: 3.
	
	final := self snapshot.
	[workingCopy backportChangesTo: base info]
		on: MCChangeSelectionRequest
		do: [:e | e resume: e patch].
		
	self assert: inst one = 2.
	self assert: inst two = 3.
	ancestry := workingCopy ancestry.
	self assert: ancestry ancestors size = 1.
	self assert: ancestry ancestors first = base info.
	self assert: ancestry stepChildren size = 1.
	self assert: ancestry stepChildren first = final info.
	
	backported := self snapshot.
	[workingCopy backportChangesTo: base info]
		on: MCChangeSelectionRequest
		do: [:e | e resume: e patch].
		
	ancestry := workingCopy ancestry.
	self assert: ancestry ancestors size = 1.
	self assert: ancestry ancestors first = base info.
	self assert: ancestry stepChildren size = 1.
	self assert: ancestry stepChildren first = backported info.
	