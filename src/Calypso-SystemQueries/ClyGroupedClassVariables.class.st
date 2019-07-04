"
I special variable query result which filter only class variables and represent them as ClyVariableMethodGroup instances in hierarchical order
"
Class {
	#name : #ClyGroupedClassVariables,
	#superclass : #ClyGroupedVariables,
	#category : #'Calypso-SystemQueries-Results'
}

{ #category : #building }
ClyGroupedClassVariables >> fillWithGroupedVariables: varsPerTypeAndDefiningClass [
	
	varsPerTypeAndDefiningClass at: ClyClassVariable ifPresent: [:classVarsPerClass |
		self fillWithVariables: classVarsPerClass type: ClyClassVariable]
]
