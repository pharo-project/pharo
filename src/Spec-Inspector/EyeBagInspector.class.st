"
Specialized version of inspector showing occurrences of items of the bag
"
Class {
	#name : #EyeBagInspector,
	#superclass : #EyeCollectionInspector,
	#category : #'Spec-Inspector'
}

{ #category : #list }
EyeBagInspector >> addVariableFields: elements [

	self sortedKeys do: [ :key |
		elements add: (BagEyeElement host: self object index: key)]
]

{ #category : #list }
EyeBagInspector >> sortedKeys [

	^ [ [ self object valuesAndCounts keys sorted ] 
		on: Error 
		do: [ "case of non sortable keys" self object valuesAndCounts keys ] 
			] on: Error do: [ #() ]
]
