tests
testSelectiveBackport
	| inst base intermediate final |
	inst := self mockInstanceA.
	base :=  self snapshot.
	self assert: inst one = 1.
	self change: #one toReturn: 2.
	intermediate := self snapshot.
	self change: #two toReturn: 3.
	final := self snapshot.
	[workingCopy backportChangesTo: base info]
		on: MCChangeSelectionRequest
		do: [:e | | patch selected |
			patch := e patch.
			selected := patch operations select: [:ea | ea definition selector = #two].
			e resume: (MCPatch operations: selected)]. 
	self assert: inst one = 1.
	self assert: inst two = 3.
	self assert: workingCopy ancestry ancestors size = 1.
	self assert: workingCopy ancestry ancestors first = base info.
	self assert: workingCopy ancestry stepChildren size = 1.
	self assert: workingCopy ancestry stepChildren first = final info