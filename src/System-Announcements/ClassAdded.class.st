"
This announcement will be emitted when a class or a trait is added in the system (when it is installed in a package)
"
Class {
	#name : 'ClassAdded',
	#superclass : 'ClassAnnouncement',
	#instVars : [
		'classAdded'
	],
	#category : 'System-Announcements-System-Classes',
	#package : 'System-Announcements',
	#tag : 'System-Classes'
}

{ #category : 'instance creation' }
ClassAdded class >> class: aClass [

	^ self new
		  classAdded: aClass;
		  yourself
]

{ #category : 'accessing' }
ClassAdded >> classAdded [
	^classAdded
]

{ #category : 'accessing' }
ClassAdded >> classAdded: aClass [
	classAdded := aClass
]

{ #category : 'accessing' }
ClassAdded >> classAffected [
	^self classAdded
]
