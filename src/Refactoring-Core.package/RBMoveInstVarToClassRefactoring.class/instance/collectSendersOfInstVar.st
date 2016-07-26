transforming
collectSendersOfInstVar
	| brokenSenders subclasses hasInstVar |
	hasInstVar := false.
	brokenSenders := OrderedCollection new.
	oldClass superclass instanceVariableNames
		do: [ :each | 
			each = variableName
				ifTrue: [ hasInstVar := true ] ].
	hasInstVar
		ifTrue: [ ^ brokenSenders ]
		ifFalse: [ 
			brokenSenders := (oldClass selectors
				select: [ :each | (oldClass methodFor: each) source includesSubstring: variableName asString ]
				thenCollect: [ :each | oldClass methodFor: each ]) asOrderedCollection.
			subclasses := oldClass subclasses.
			subclasses
				do: [ :subclass | 
					subclass selectors
						do: [ :each | 
							((subclass methodFor: each) source includesSubstring: ' ' , variableName asString , ' ')
								ifTrue: [ brokenSenders add: (subclass methodFor: each) ] ] ].
			^ brokenSenders ]