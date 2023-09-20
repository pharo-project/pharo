"
the announcement will be emitted when removing a class or a trait using:  
	=> removeFromSystem
"
Class {
	#name : 'ClassRemoved',
	#superclass : 'ClassAnnouncement',
	#instVars : [
		'classRemoved',
		'packageTag'
	],
	#category : 'System-Announcements-System-Classes',
	#package : 'System-Announcements',
	#tag : 'System-Classes'
}

{ #category : 'instance creation' }
ClassRemoved class >> class: aClass [

	^ self new
		  classRemoved: aClass;
		  yourself
]

{ #category : 'instance creation' }
ClassRemoved class >> class: aClass packageTag: aPackageTag [

	^ (self class: aClass)
		  packageTag: aPackageTag;
		  yourself
]

{ #category : 'testing' }
ClassRemoved >> affectsMethod: aMethod [
	^classRemoved == aMethod origin
]

{ #category : 'testing' }
ClassRemoved >> affectsMethods [
	"methods are removed together with removed class"
	^true
]

{ #category : 'testing' }
ClassRemoved >> affectsMethodsDefinedInClass: aClass [

	^classRemoved == aClass
]

{ #category : 'testing' }
ClassRemoved >> affectsMethodsDefinedInPackage: aPackage [

	^ self packageAffected == aPackage
]

{ #category : 'testing' }
ClassRemoved >> affectsMethodsInProtocol: protocol [

	^ classRemoved protocolNames includes: protocol
]

{ #category : 'accessing' }
ClassRemoved >> classAffected [
	^self classRemoved
]

{ #category : 'accessing' }
ClassRemoved >> classRemoved [

	^ classRemoved
]

{ #category : 'accessing' }
ClassRemoved >> classRemoved: aClass [

	classRemoved := aClass.
	packageTag := aClass packageTag
]

{ #category : 'accessing' }
ClassRemoved >> packageAffected [

	^ self packageTag package
]

{ #category : 'accessing' }
ClassRemoved >> packageTag [

	^ packageTag
]

{ #category : 'accessing' }
ClassRemoved >> packageTag: anObject [

	packageTag := anObject
]
