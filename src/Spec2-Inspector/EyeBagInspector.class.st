"
Specialized version of inspector showing occurrences of items of the bag
"
Class {
	#name : #EyeBagInspector,
	#superclass : #EyeCollectionInspector,
	#category : #'Spec2-Inspector-Base'
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
