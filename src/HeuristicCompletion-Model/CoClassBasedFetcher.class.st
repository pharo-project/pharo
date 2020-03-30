Class {
	#name : #CoClassBasedFetcher,
	#superclass : #CoFetcher,
	#instVars : [
		'completionClass',
		'astNode'
	],
	#category : #'HeuristicCompletion-Model-Fetchers'
}

{ #category : #testing }
CoClassBasedFetcher class >> isAbstract [

	^ self == CoClassBasedFetcher
]

{ #category : #accessing }
CoClassBasedFetcher >> astNode [
	^ astNode
]

{ #category : #accessing }
CoClassBasedFetcher >> astNode: anObject [
	astNode := anObject
]

{ #category : #accessing }
CoClassBasedFetcher >> completionClass [
	^ completionClass
]

{ #category : #accessing }
CoClassBasedFetcher >> completionClass: anObject [
	completionClass := anObject
]

{ #category : #'hierarchy-fetching' }
CoClassBasedFetcher >> forHierarchy [
	
	^ CoHierarchyFetcher new
		decoree: self;
		yourself
]

{ #category : #'hierarchy-fetching' }
CoClassBasedFetcher >> onSuperclass [
	
	completionClass superclass
		ifNil: [ ^ CoEmptyFetcher new ].

	^ self copy
		completionClass: completionClass superclass;
		yourself
]
