"
I represent all possible changes with class.
I am used to subscribe with single subscription to any changes related to classes.
So you can write code like this: 

	SystemAnnouncer uniqueInstance when: (ClyClassChange of: MyClass) do: [ :ann | ann logCr ].
	
I provide more general subscription logic than subscribing on base ClassAnnouncement class. Many other events can be also related to class changes like renaming package or running class tests. They are not covered by ClassAnnouncement hierarchy.
I reuse logic of queries how they detect that their result is affected by system changes: 
	
	ClyClassChange>>handlesAnnouncement: anAnnouncement 
 		^anAnnouncement affectsClass: affectedClass	

I am used by class editor tools to be updated when editing class is changed.
	
Internal Representation and Key Implementation Points.

    Instance Variables
	affectedClass:		<Class>
"
Class {
	#name : #ClyClassChange,
	#superclass : #Object,
	#instVars : [
		'affectedClass'
	],
	#category : #'Calypso-SystemQueries-Domain'
}

{ #category : #'instance creation' }
ClyClassChange class >> of: aClass [
	^self new 
		affectedClass: aClass
]

{ #category : #accessing }
ClyClassChange >> affectedClass [
	^ affectedClass
]

{ #category : #accessing }
ClyClassChange >> affectedClass: anObject [
	affectedClass := anObject
]

{ #category : #testing }
ClyClassChange >> handlesAnnouncement: anAnnouncement [

	^anAnnouncement affectsClass: affectedClass
]
