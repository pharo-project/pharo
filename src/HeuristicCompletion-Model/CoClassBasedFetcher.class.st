"
I am an abstract complishon fetcher that looks for things inside a class.
My subclasses will, for example, fetch a class' class variables, messages implemented in the class, instance variables.

I am explicitly created with the superclass I will look into.
This allows other combinators to act upon me.
- I define #onSuperclass that creates a copy of myself but acting on my superclass.
- I define #forHierarchy that decorates myself with a HierarchyComplishonFetcher that makes a search for the entire hierarchy.
"
Class {
	#name : 'CoClassBasedFetcher',
	#superclass : 'CoFetcher',
	#instVars : [
		'completionClass',
		'astNode'
	],
	#category : 'HeuristicCompletion-Model-Fetchers',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Fetchers'
}

{ #category : 'testing' }
CoClassBasedFetcher class >> isAbstract [

	^ self == CoClassBasedFetcher
]

{ #category : 'accessing' }
CoClassBasedFetcher >> astNode [
	^ astNode
]

{ #category : 'accessing' }
CoClassBasedFetcher >> astNode: anObject [
	astNode := anObject
]

{ #category : 'accessing' }
CoClassBasedFetcher >> completionClass [
	^ completionClass
]

{ #category : 'accessing' }
CoClassBasedFetcher >> completionClass: anObject [
	completionClass := anObject
]

{ #category : 'hierarchy-fetching' }
CoClassBasedFetcher >> forHierarchy [

	^ CoHierarchyFetcher new
		decoree: self;
		yourself
]

{ #category : 'hierarchy-fetching' }
CoClassBasedFetcher >> onSuperclass [

	completionClass superclass
		ifNil: [ ^ CoEmptyFetcher new ].

	^ self copy
		completionClass: completionClass superclass;
		yourself
]
