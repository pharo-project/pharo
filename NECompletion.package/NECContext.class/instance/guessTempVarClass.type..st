private
guessTempVarClass: aString type: aSymbol 
	| current type varName varType sourceIndex aClass |
	aClass := controller guessTypeFor: aString.
	aClass ifNotNil: [ ^ aClass ].
	varName := aString.
	varType := aSymbol.
	1 
		to: completionIndex
		do: 
			[ :index | 
			current := ranges at: index.
			(current type = varType and: [ (self sourceOf: current) = varName and: [ index + 3 <= ranges size ] ]) ifTrue: 
				[ (sourceIndex := self findSourceRangeFor: index) notNil ifTrue: 
					[ type := self guessTypeOf: sourceIndex.
					type ifNotNil: [ ^ type ] ] ] ].
	^ nil