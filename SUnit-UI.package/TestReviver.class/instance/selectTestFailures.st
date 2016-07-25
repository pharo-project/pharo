accessing
selectTestFailures	
	| files results tokens method | 
	results := OrderedCollection new.
	files := FileLocator imageDirectory filesMatching: '*.fuel'.
	files do: [:aFile |
		tokens := aFile resolve base findTokens: '-'.	
		method := (Smalltalk globals at: tokens first asSymbol ) lookupSelector:  tokens second asSymbol.
		method pragmas isEmpty
			ifTrue: [ results add: aFile ]
			ifFalse:  [
				(method pragmas first keyword = #expectedFailure) 
					ifFalse: [ results add: aFile ].
				 ]
	].
	^ results