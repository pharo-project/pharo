Class {
	#name : 'RBHierarchySubclassAForClassVariable',
	#superclass : 'RBHierarchyForClassVariable',
	#classVars : [
		'ClassVarA'
	],
	#category : 'Refactoring-DataForTesting-MiniHierarchy',
	#package : 'Refactoring-DataForTesting',
	#tag : 'MiniHierarchy'
}

{ #category : 'initialization' }
RBHierarchySubclassAForClassVariable >> initialize [
	<ignoreUnusedClassVariables: #( ClassVarA )>
	super initialize.

]
