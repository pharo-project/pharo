"
Specialized version of inspector showing occurrences of items of the bag
"
Class {
	#name : #SpEyeBagInspector,
	#superclass : #SpEyeCollectionInspector,
	#category : #'Spec2-Inspector-Base'
}

{ #category : #list }
SpEyeBagInspector >> addVariableFields: elements [

	self sortedKeys do: [ :key |
		elements add: (SpBagEyeElement host: self object index: key)]
]

{ #category : #list }
SpEyeBagInspector >> sortedKeys [

	^ [ [ self object valuesAndCounts keys sorted ] 
		on: Error 
		do: [ "case of non sortable keys" self object valuesAndCounts keys ] 
			] on: Error do: [ #() ]
]
