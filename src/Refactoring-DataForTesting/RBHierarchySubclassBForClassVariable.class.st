Class {
	#name : 'RBHierarchySubclassBForClassVariable',
	#superclass : 'RBHierarchySubclassAForClassVariable',
	#classVars : [
		'ClassVarB'
	],
	#category : 'Refactoring-DataForTesting-MiniHierarchy',
	#package : 'Refactoring-DataForTesting',
	#tag : 'MiniHierarchy'
}

{ #category : 'initialization' }
RBHierarchySubclassBForClassVariable >> initialize [

	<ignoreUnusedClassVariables: #( ClassVarB )>
	super initialize.

]
