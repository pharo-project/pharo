"
I am a RBBrowserEnvironment on a set of classes of a class hierarchy.

Example:

(RBBrowserEnvironment new) forClass:Morph protocols:{'printing'}.
"
Class {
	#name : 'RBClassHierarchyEnvironment',
	#superclass : 'RBBrowserEnvironmentWrapper',
	#instVars : [
		'class'
	],
	#category : 'Refactoring-Environment',
	#package : 'Refactoring-Environment'
}

{ #category : 'instance creation' }
RBClassHierarchyEnvironment class >> class: aClass [
	^ self
		onEnvironment: self default
		class: aClass
]

{ #category : 'accessing' }
RBClassHierarchyEnvironment class >> defaultName [
	^ 'Hier'
]

{ #category : 'instance creation' }
RBClassHierarchyEnvironment class >> onEnvironment: anEnvironment class: aClass [
	^ (self onEnvironment: anEnvironment)
		class: aClass;
		yourself
]

{ #category : 'accessing' }
RBClassHierarchyEnvironment >> basisObjects [
	^ { class }
]

{ #category : 'accessing' }
RBClassHierarchyEnvironment >> class: aClass [
	class := aClass
]

{ #category : 'testing' }
RBClassHierarchyEnvironment >> definesClass: aClass [
	^ (aClass == class or:
		[ (class inheritsFrom: aClass) or:
			[ aClass inheritsFrom: class ] ]) and: [super definesClass: aClass]
]

{ #category : 'testing' }
RBClassHierarchyEnvironment >> includesClass: aClass [
	^ (aClass == class or:
		[ (class inheritsFrom: aClass) or:
			[ aClass inheritsFrom: class ] ]) and: [super includesClass: aClass]
]
