preconditions
checkReferencesToAnyOf: aSelectorCollection
	| occurrences |
	occurrences := OrderedCollection new.
	aSelectorCollection
		do: [ :each | 
			self model
				allReferencesTo: each
				do: [ :aRBMethod | 
					(aSelectorCollection includes: aRBMethod selector)
						ifFalse: [ occurrences add: each -> aRBMethod ] ] ].
	occurrences ifEmpty: [ ^ self ].
	^ self checkBrowseOccurrences: occurrences