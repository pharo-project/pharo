"
the announcement will be emitted when removing a class or a trait using:  
	=> removeFromSystem
"
Class {
	#name : #ClassRemoved,
	#superclass : #ClassAnnouncement,
	#instVars : [
		'categoryName',
		'classRemoved',
		'affectedPackage'
	],
	#category : #'System-Announcements-System-Classes'
}

{ #category : #'instance creation' }
ClassRemoved class >> class: aClass category: aCategoryName [
	^self new
			classRemoved: aClass;
			categoryName: aCategoryName;
			yourself
]

{ #category : #'instance creation' }
ClassRemoved class >> class: aClass package: aPackage category: aCategoryName [

	^ (self class: aClass category: aCategoryName)
		  affectedPackage: aPackage;
		  yourself
]

{ #category : #accessing }
ClassRemoved >> affectedPackage: anObject [
	affectedPackage := anObject
]

{ #category : #testing }
ClassRemoved >> affectsMethod: aMethod [
	^classRemoved == aMethod origin
]

{ #category : #testing }
ClassRemoved >> affectsMethods [
	"methods are removed together with removed class"
	^true
]

{ #category : #testing }
ClassRemoved >> affectsMethodsDefinedInClass: aClass [

	^classRemoved == aClass
]

{ #category : #testing }
ClassRemoved >> affectsMethodsDefinedInPackage: aPackage [

	^affectedPackage == aPackage
]

{ #category : #testing }
ClassRemoved >> affectsMethodsInProtocol: protocol [

	^ classRemoved protocolNames includes: protocol
]

{ #category : #accessing }
ClassRemoved >> categoryName [

	^ categoryName
]

{ #category : #accessing }
ClassRemoved >> categoryName: anObject [

	categoryName := anObject
]

{ #category : #accessing }
ClassRemoved >> classAffected [
	^self classRemoved
]

{ #category : #accessing }
ClassRemoved >> classRemoved [

	^ classRemoved
]

{ #category : #accessing }
ClassRemoved >> classRemoved: aClass [

	classRemoved := aClass.
	affectedPackage := aClass package
]

{ #category : #accessing }
ClassRemoved >> classTagAffected [
	^affectedPackage toTagName: categoryName
]

{ #category : #accessing }
ClassRemoved >> packageAffected [

	^ affectedPackage
]

{ #category : #accessing }
ClassRemoved >> packageAffected: aPackage [

	affectedPackage := aPackage
]
