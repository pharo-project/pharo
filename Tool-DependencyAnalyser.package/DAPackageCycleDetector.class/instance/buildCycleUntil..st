actions
buildCycleUntil: children
	| aPackageCycle currentNode |
	aPackageCycle := DAPackageCycle new.
	currentNode := children.
	[ currentNode isNil ]
		whileFalse: [ 
			aPackageCycle addPackage: currentNode.
			currentNode := currentNode bfsParent ].
	(self cycleAlreadyFound: aPackageCycle)
		ifFalse: [ 
			aPackageCycle reversedCycle.
			self addCycle: aPackageCycle ]