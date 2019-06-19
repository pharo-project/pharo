"
I represent all possible changes with package.
I am used to subscribe with single subscription to any changes related to packages.
So you can write code like this: 

	SystemAnnouncer uniqueInstance when: (ClyPackageChange of: aPackage) do: [ :ann | ann logCr ].
	
I provide more general subscription logic than subscribing on base RPackageAnnouncement class. Many other events can be also related to package changes like class creation. They are not covered by RPackageAnnouncement hierarchy.
I reuse logic of queries how they detect that their result is affected by system changes: 
	
	ClyPackageChange>>handlesAnnouncement: anAnnouncement 
 		^anAnnouncement affectsPackage: affectedPackage	

I am used by class editor tools to be updated when editing class is changed.
	
Internal Representation and Key Implementation Points.

    Instance Variables
	affectedPackage:		<RPackage>
"
Class {
	#name : #ClyPackageChange,
	#superclass : #Object,
	#instVars : [
		'affectedPackage'
	],
	#category : #'Calypso-SystemQueries-Domain'
}

{ #category : #'instance creation' }
ClyPackageChange class >> of: aPackage [
	^self new 
		affectedPackage: aPackage
]

{ #category : #accessing }
ClyPackageChange >> affectedPackage [
	^ affectedPackage
]

{ #category : #accessing }
ClyPackageChange >> affectedPackage: anObject [
	affectedPackage := anObject
]

{ #category : #testing }
ClyPackageChange >> handlesAnnouncement: anAnnouncement [

	^anAnnouncement affectsPackage: affectedPackage
]
