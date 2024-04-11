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
			flatCollect: [ :aClass | aClass methods]) 
]

{ #category : 'query' }
EquivalentTreeChecker >> methodsToBeChecked [

	^ self allMethodsInHierarchy reject: [ :m | m selector = selector ]
]

{ #category : 'instance creation' }
EquivalentTreeChecker >> model: aModel [ 
	
	model := aModel
]

{ #category : 'instance creation' }
EquivalentTreeChecker >> on: aClass [ 
	
	class := model classNamed: aClass name
]
