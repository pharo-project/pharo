scope
popScope
	| scope tempIndex |
	scope := self scope.
	
	irBuilder addTemps: scope temps.
	
	" Remap your own temp vectors "
	scope ownTempVectors do: [ :tempVector |
		irBuilder
			remapTemp: (scope -> tempVector index)
			toRemote: tempVector ].
	
	" Remap the copied values "
	tempIndex := scope numArgs.
	scope copiedValues do: [ :copiedValue |
		irBuilder
			remapTemp: (scope -> tempIndex)
			toRemote: copiedValue.
		tempIndex := tempIndex + 1. ].
	
	^ scopeStack pop