protocol
selectNextMethod
	| methodToSelect selectedMethodIndex |
	self getMethods size <= 1
		ifFalse:
			[ 
			selectedMethodIndex := self selectedMethodIndex.
			methodToSelect := selectedMethodIndex < self getMethods size
				ifTrue: [ self getMethods at: selectedMethodIndex + 1 ]
				ifFalse: [ self getMethods at: selectedMethodIndex - 1 ] ].
	self selectMethod: methodToSelect.
	self model selectedMethod: methodToSelect