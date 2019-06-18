"
I represent event when superclass (overriddenSuperclass) of some classes is changed. In that case all subclasses which could override his methods are also changed: their methods should update the ""override status"".

Read more details in superclass comment.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	overriddenSuperclass:		<Class>

"
Class {
	#name : #ClyOverridingSubclassesChanged,
	#superclass : #ClyClassHierarchyChanged,
	#instVars : [
		'overriddenSuperclass'
	],
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Queries'
}

{ #category : #'instance creation' }
ClyOverridingSubclassesChanged class >> overriddenSuperclass: aClass [

	^self new 
		overriddenSuperclass: aClass
]

{ #category : #testing }
ClyOverridingSubclassesChanged >> affectsMethodsDefinedInClass: aClass [

	^aClass instanceSide includesBehavior: overriddenSuperclass
]

{ #category : #testing }
ClyOverridingSubclassesChanged >> affectsMethodsDefinedInPackage: aPackage [

	overriddenSuperclass withAllSubclassesDo: [:each | 
		(each package = aPackage) ifTrue: [ ^true ] ].

	^false
]

{ #category : #testing }
ClyOverridingSubclassesChanged >> affectsMethodsTaggedWith: tagName [

	overriddenSuperclass withAllSubclassesDo: [:each | 
		(each tagsForMethods includes: tagName) ifTrue: [ ^true ] ].

	^false
]

{ #category : #accessing }
ClyOverridingSubclassesChanged >> classAffected [
	^ overriddenSuperclass
]

{ #category : #accessing }
ClyOverridingSubclassesChanged >> overriddenSuperclass [
	^ overriddenSuperclass
]

{ #category : #accessing }
ClyOverridingSubclassesChanged >> overriddenSuperclass: anObject [
	overriddenSuperclass := anObject
]
