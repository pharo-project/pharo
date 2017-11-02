"
I am a common superclass for package, class, protocol and method selection. I contain all the state and functionality, my subclasses are there only to allow the distinction between announcements
"
Class {
	#name : #NautilusSelectionAnnouncement,
	#superclass : #Announcement,
	#instVars : [
		'package',
		'class',
		'protocol',
		'method'
	],
	#category : 'NautilusCommon-Announcements'
}

{ #category : #compatibility }
NautilusSelectionAnnouncement >> category [

	^ self selectedProtocol
]

{ #category : #compatibility }
NautilusSelectionAnnouncement >> itemClass [

	^ self selectedClass 
]

{ #category : #compatibility }
NautilusSelectionAnnouncement >> method [

	^ self selectedMethod 
]

{ #category : #compatibility }
NautilusSelectionAnnouncement >> package [

	^ self selectedPackage 
]

{ #category : #accessing }
NautilusSelectionAnnouncement >> selectedClass [
	^ class
]

{ #category : #accessing }
NautilusSelectionAnnouncement >> selectedClass: anObject [
	class := anObject
]

{ #category : #accessing }
NautilusSelectionAnnouncement >> selectedMethod [
	^ method
]

{ #category : #accessing }
NautilusSelectionAnnouncement >> selectedMethod: anObject [
	method := anObject
]

{ #category : #accessing }
NautilusSelectionAnnouncement >> selectedPackage [
	^ package
]

{ #category : #accessing }
NautilusSelectionAnnouncement >> selectedPackage: anObject [
	package := anObject
]

{ #category : #accessing }
NautilusSelectionAnnouncement >> selectedProtocol [
	^ protocol
]

{ #category : #accessing }
NautilusSelectionAnnouncement >> selectedProtocol: anObject [
	protocol := anObject
]
