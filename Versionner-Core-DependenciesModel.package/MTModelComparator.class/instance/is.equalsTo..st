comparing
is: anMTProject equalsTo: anOtherMTProject
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
