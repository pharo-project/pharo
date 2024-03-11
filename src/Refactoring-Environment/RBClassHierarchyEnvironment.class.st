"
I am a RBBrowserEnvironment on a set of classes of a class hierarchy.

Example:

(RBBrowserEnvironment new) forClass:Morph protocols:{'printing'}.
"
Class {
	#name : #RBClassHierarchyEnvironment,
	#superclass : #RBBrowserEnvironmentWrapper,
	#instVars : [
		#class
	],
	#category : #'Refactoring-Environment'
}

{ #category : #'instance creation' }
RBClassHierarchyEnvironment class >> class: aClass [
	^ self
		onEnvironment: self default
		class: aClass
]

{ #category : #accessing }
RBClassHierarchyEnvironment class >> defaultName [
	^ 'Hier'
]

{ #category : #'instance creation' }
RBClassHierarchyEnvironment class >> onEnvironment: anEnvironment class: aClass [
	^ (self onEnvironment: anEnvironment)
		class: aClass;
		yourself
]

{ #category : #accessing }
RBClassHierarchyEnvironment >> basisObjects [
	^ { class }
]

{ #category : #accessing }
RBClassHierarchyEnvironment >> class: aClass [
	class := aClass
]

{ #category : #testing }
RBClassHierarchyEnvironment >> definesClass: aClass [
	^ (aClass == class or:
		[ (class inheritsFrom: aClass) or:
			[ aClass inheritsFrom: class ] ]) and: [super definesClass: aClass]
]

{ #category : #testing }
RBClassHierarchyEnvironment >> includesCategory: aCategory [
	^ (super includesCategory: aCategory) and: [
		(environment classNamesFor: aCategory)
			inject: false into: [ :bool :each |
				bool or: [
					| aClass |
					aClass := self systemDictionary at: each ifAbsent: [ nil ].
					aClass == class or: [ aClass class == class ] ] ] ]
]

{ #category : #testing }
RBClassHierarchyEnvironment >> includesClass: aClass [
	^ (aClass == class or:
		[ (class inheritsFrom: aClass) or:
			[ aClass inheritsFrom: class ] ]) and: [super includesClass: aClass]
]

{ #category : #accessing }
RBClassHierarchyEnvironment >> packages [
	^ self classes collect: [:each | each package]
]
