"
I am similar to ClySuperclasScope but I restrict superclasses chain up to Object which is not interesting command class for many browser scenarios.
So I show all superclasses, their methods and variables except Object, ProtoObject and all their related stuff.

You can restrict any class scope to be interesting scope using convertation method: 

	aClassScope asInterestingClassScope 

By default it will return class scope itself. But in case when given scope consist of superclass scope it will be replaced by my instance
"
Class {
	#name : #ClyInterestingSuperclassScope,
	#superclass : #ClySingleClassRelationScope,
	#category : #'Calypso-SystemQueries-Scopes'
}

{ #category : #accessing }
ClyInterestingSuperclassScope class >> classesRelatedTo: aClass do: aBlock [
	| nextSuperclass |
	aClass isRootInEnvironment ifTrue: [ ^self ].
	
	nextSuperclass := aClass superclass.
	nextSuperclass = Object ifTrue: [ ^self ].
	
	aBlock value: nextSuperclass
]

{ #category : #accessing }
ClyInterestingSuperclassScope class >> defaultName [
	^'superclasses'
]
