"
I am a browser environment whose system is limited to a given set of classes. Tests use me as their universe: negating or intersecting an environment then walks only those classes instead of the whole image, which is much faster.

Instance creation: `RBScopedBrowserEnvironment onClasses: aCollectionOfClasses`.

Public API: I have the protocol of my superclass.

Internal representation: `scope` is a system dictionary containing only the bindings of my classes. The environments wrapping me enumerate classes through it.

Collaborators: `RBBrowserEnvironment` and its wrappers.
"
Class {
	#name : 'RBScopedBrowserEnvironment',
	#superclass : 'RBBrowserEnvironment',
	#instVars : [
		'scope'
	],
	#category : 'Refactoring-Environment-Tests-Utilities',
	#package : 'Refactoring-Environment-Tests',
	#tag : 'Utilities'
}

{ #category : 'instance creation' }
RBScopedBrowserEnvironment class >> onClasses: aCollection [

	^ self new
		  setClasses: aCollection;
		  yourself
]

{ #category : 'testing' }
RBScopedBrowserEnvironment >> includesClass: aClass [

	^ (scope at: aClass instanceSide name ifAbsent: [ nil ]) == aClass instanceSide
]

{ #category : 'initialization' }
RBScopedBrowserEnvironment >> setClasses: aCollection [

	scope := SystemEnvironment new.
	aCollection do: [ :class | scope add: class instanceSide binding ]
]

{ #category : 'private' }
RBScopedBrowserEnvironment >> systemDictionary [

	^ scope
]
