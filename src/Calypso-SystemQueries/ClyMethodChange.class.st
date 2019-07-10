"
I represent all possible changes with method.
I am used to subscribe with single subscription to any changes related to methods.
So you can write code like this: 

	SystemAnnouncer uniqueInstance when: (ClyMethodChange of: MyClass >> #someMethod) do: [ :ann | ann logCr ].
	
I provide more general subscription logic than subscribing on base MethodAnnouncement class. Many other events can be also related to method changes like renaming package or running test. They are not covered by MethodAnnouncement hierarchy.
I reuse logic of queries how they detect that their result is affected by system changes: 
	
	ClyMethodChange>>handlesAnnouncement: anAnnouncement 
 		^anAnnouncement affectsMethod: affectedMethod	

Also I provide class side method to get AnnouncementSet representing multiple method changes: 

	ClyMethodChange ofAll: { MyClass >> #someMethod. MyClass >> #someMethod2 }.

I am used by method editor tools to be updated when editing method is changed.
	
Internal Representation and Key Implementation Points.

    Instance Variables
	affectedMethod:		<CompiledMethod>
"
Class {
	#name : #ClyMethodChange,
	#superclass : #Object,
	#instVars : [
		'affectedMethod'
	],
	#category : #'Calypso-SystemQueries-Domain'
}

{ #category : #'instance creation' }
ClyMethodChange class >> of: aMethod [
	^self new 
		affectedMethod: aMethod
]

{ #category : #'instance creation' }
ClyMethodChange class >> ofAll: methods [
	^methods collect: [ :each | self of: each ] as: AnnouncementSet 
]

{ #category : #accessing }
ClyMethodChange >> affectedMethod [
	^ affectedMethod
]

{ #category : #accessing }
ClyMethodChange >> affectedMethod: anObject [
	affectedMethod := anObject
]

{ #category : #testing }
ClyMethodChange >> handlesAnnouncement: anAnnouncement [

	| newMethod |
	affectedMethod isInstalled ifFalse: [ 
		newMethod := affectedMethod origin 
			localMethodNamed: affectedMethod selector ifAbsent: [nil].
		newMethod ifNotNil: [ affectedMethod := newMethod ]  ].
	
	^anAnnouncement affectsMethod: affectedMethod 
]
