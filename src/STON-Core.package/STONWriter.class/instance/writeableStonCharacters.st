private
writeableStonCharacters
	^ self stonCharacters == STONCharacters
		ifTrue: [ stonCharacters := stonCharacters copy ]
		ifFalse: [ stonCharacters ]