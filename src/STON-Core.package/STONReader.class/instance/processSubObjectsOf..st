private
processSubObjectsOf: object
	| unresolvedReferencesCount |
	unresolvedReferencesCount := unresolvedReferences.
	object stonProcessSubObjects: [ :each | 
		each isStonReference
			ifTrue: [ self resolveReference: each ]
			ifFalse: [ 
				each stonContainSubObjects
					ifTrue: [ self processSubObjectsOf: each ]
					ifFalse: [ each ] ] ].
	unresolvedReferencesCount > unresolvedReferences
		ifTrue: [ object stonPostReferenceResolution ].
	^ object