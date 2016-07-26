selection
selectedMethodIndex: anInteger

	| aMethod |
	aMethod :=  self getMethods at: anInteger ifAbsent: [ nil ].
	self selectedMethod: aMethod.

	self changed: #selectedMethodIndex.