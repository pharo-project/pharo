"
A comparator that can be used to compare a whole dependency graph.
"
Class {
	#name : #MTModelComparator,
	#superclass : #Object,
	#category : #Versionner-Core-DependenciesModel
}

{ #category : #protocol }
MTModelComparator class >> is: anMTProject equalsTo: anOtherMTProject [
	^self new is: anMTProject equalsTo: anOtherMTProject
]

{ #category : #comparing }
MTModelComparator >> is: anMTProject equalsTo: anOtherMTProject [
	| referenceModelIterator modelToCompareIterator |
	
	referenceModelIterator := MTProjectBFSIterator on: anMTProject.
	modelToCompareIterator := MTProjectBFSIterator on: anOtherMTProject.
	
	[ true ] whileTrue: [ | referenceElement elementToCompare |
			referenceElement := referenceModelIterator next.
			elementToCompare := modelToCompareIterator next.
			(referenceElement = elementToCompare ) 
				ifFalse: [ ^false ].
			referenceModelIterator hasNext 
				ifFalse: [ ^ modelToCompareIterator hasNext not ]  ]

]
