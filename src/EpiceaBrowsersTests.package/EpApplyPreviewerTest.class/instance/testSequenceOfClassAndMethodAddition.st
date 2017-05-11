tests - multiple changes
testSequenceOfClassAndMethodAddition

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'number'.
	self setMonitorLogAsInputEntries.

	aClass removeSelector: #fortyTwo.
	aClass removeFromSystem.

	self assertOutputsEventsWith: [:output | 
		| outputClasses |
		outputClasses := output collect: #class as: Array.
		self assert: outputClasses equals: { EpClassAddition. EpMethodAddition }.
		]
