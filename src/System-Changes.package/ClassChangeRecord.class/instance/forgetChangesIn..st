removal
forgetChangesIn: otherRecord
	"See forgetAllChangesFoundIn:.  Used in culling changeSets."

	| cls otherMethodChanges |
	(cls := self realClass) ifNil: [ ^ self ].	"We can do better now, though..."
	otherMethodChanges := otherRecord methodChangeTypes.
	otherMethodChanges
		associationsDo: [ :assoc | 
			| selector actionToSubtract |
			selector := assoc key.
			actionToSubtract := assoc value.
			(cls includesSelector: selector)
				ifTrue: [ 
					(#(#add #change) includes: actionToSubtract)
						ifTrue: [ methodChanges removeKey: selector ifAbsent: [  ] ] ]
				ifFalse: [ 
					(#(#remove #addedThenRemoved) includes: actionToSubtract)
						ifTrue: [ methodChanges removeKey: selector ifAbsent: [  ] ] ] ].
	changeTypes isEmpty
		ifTrue: [ ^ self ].
	changeTypes removeAllFoundIn: otherRecord allChangeTypes.
	(changeTypes includes: #rename)
		ifFalse: [ changeTypes removeAllSuchThat: [ :x | x beginsWith: 'oldName: ' ] ]