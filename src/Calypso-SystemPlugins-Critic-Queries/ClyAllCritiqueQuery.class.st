"
I am abstract critique query which subclasses are supposed to return all critiques abailable from scope.
It is possible to retrieve critiques differently from objects which are visible from scope.
My subclasses define concrete way but all of them return all critiques without any extra condition.
They should implement method: 

- analyzedObjectsDo: aBlock
"
Class {
	#name : #ClyAllCritiqueQuery,
	#superclass : #ClyCritiqueQuery,
	#category : #'Calypso-SystemPlugins-Critic-Queries'
}

{ #category : #execution }
ClyAllCritiqueQuery >> analyzedObjectsDo: aBlock [
	self subclassResponsibility 
]

{ #category : #execution }
ClyAllCritiqueQuery >> buildResult: aQueryResult [
		
	| result |
	result := OrderedCollection new.
	
	self analyzedObjectsDo: [ :each | result addAll: each critiques].

	aQueryResult fillWith: result
]

{ #category : #execution }
ClyAllCritiqueQuery >> checkEmptyResult [
	
	self analyzedObjectsDo: [ :each | 
		each critiques ifNotEmpty: [ ^false ] ].
	
	^true
]

{ #category : #printing }
ClyAllCritiqueQuery >> description [

	^'all critiques'
]
