Class {
	#name : 'EquivalentTreeChecker',
	#superclass : 'Object',
	#instVars : [
		'selector',
		'class',
		'model'
	],
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'query' }
EquivalentTreeChecker >> allMethodsInHierarchy [
	^ ((class withAllSuperclassesUntil: Object) 
			flatCollect: [ :class | class methods]) 
]

{ #category : 'as yet unclassified' }
EquivalentTreeChecker >> allMethodsInHierarchyOf: aRBClass [

	^ ((aRBClass withAllSuperclassesUntil: Object) 
			flatCollect: [ :class | class methods collect: [ :each | each method ] ]) 
]

{ #category : 'as yet unclassified' }
EquivalentTreeChecker >> methodsToBeChecked [

	^ (self allMethodsInHierarchyOf: self definingClass) reject: [
		  :m | m selector = selector ]
]

{ #category : 'instance creation' }
EquivalentTreeChecker >> model: aModel [ 
	
	model := aModel
]

{ #category : 'instance creation' }
EquivalentTreeChecker >> on: aClass [ 
	
	class := model classNamed: aClass name
]

{ #category : 'instance creation' }
EquivalentTreeChecker >> rbClassFor: aClass [

	^ model classNamed: aClass name
]
