as yet unclassified
openMenuIn: aBlock
	| alternatives labels lines caption choice |
	alternatives := Symbol possibleSelectorsFor: name.
	labels := Array streamContents:
				[:s | s nextPut: name; nextPutAll: alternatives; nextPut: 'cancel'].
	lines := {1. alternatives size + 1}.
	caption := 'Unknown selector, please\confirm, correct, or cancel' withCRs.
	
	choice := aBlock value: labels value: lines value: caption.
	choice = 0 ifTrue: [self resume: nil].
	choice = 1 ifTrue: [self resume: name asSymbol].
	choice = labels size ifTrue: [self resume: nil].
	self resume: (alternatives at: choice - 1)