"
I represent event when some class (overridingSubclass) is changed. In that case all superclasses are changed too because all their methods should update the ""overridden status"".

Read more details in superclass comment.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	overridingSubclass:		<Class>
"
Class {
	#name : #ClyOverriddenSuperclassesChanged,
	#superclass : #ClyClassHierarchyChanged,
	#instVars : [
		'overridingSubclass'
	],
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Queries'
}

{ #category : #'instance creation' }
ClyOverriddenSuperclassesChanged class >> overridingSubclass: aClass [
	^self new 
		overridingSubclass: aClass
]

{ #category : #testing }
ClyOverriddenSuperclassesChanged >> affectsMethodsDefinedInClass: aClass [

	^overridingSubclass inheritsFrom: aClass instanceSide
]

{ #category : #testing }
ClyOverriddenSuperclassesChanged >> affectsMethodsDefinedInPackage: aPackage [

	overridingSubclass allSuperclassesDo: [:each | 
		(each package = aPackage) ifTrue: [ ^true ] ].

	^false
]

{ #category : #testing }
ClyOverriddenSuperclassesChanged >> affectsMethodsTaggedWith: tagName [

	overridingSubclass allSuperclassesDo: [:each | 
		(each tagsForMethods includes: tagName) ifTrue: [ ^true ] ].

	^false
]

{ #category : #accessing }
ClyOverriddenSuperclassesChanged >> classAffected [
	^overridingSubclass
]

{ #category : #accessing }
ClyOverriddenSuperclassesChanged >> overridingSubclass [
	^overridingSubclass
]

{ #category : #accessing }
ClyOverriddenSuperclassesChanged >> overridingSubclass: aClass [
	overridingSubclass := aClass
]
