"
I am an abstract class that represents a system event.
"
Class {
	#name : #EpEvent,
	#superclass : #Object,
	#category : #'Epicea-Model'
}

{ #category : #announcements }
EpEvent class >> handlesAnnouncement: anAnnouncement [
	"The receiver acts as a filter to determine whether subscribers who used the receiver as signaling tag (event identifier class or symbol) should receive incoming announcement. In particular, registering to a superclass will receive the announcements from all subclasses."

	^ anAnnouncement class includesBehavior: self
]

{ #category : #visitor }
EpEvent >> accept: aVisitor [

	self subclassResponsibility 
]

{ #category : #testing }
EpEvent >> isCodeChange [
	^ false
]

{ #category : #testing }
EpEvent >> isEpBehaviorChange [

	^ false
]

{ #category : #testing }
EpEvent >> isEpCategoryChange [

	^ false
]

{ #category : #testing }
EpEvent >> isEpClassChange [

	^ false
]

{ #category : #testing }
EpEvent >> isEpEvent [

	^ true
]

{ #category : #testing }
EpEvent >> isEpLogEntriesComment [

	^ false
]

{ #category : #testing }
EpEvent >> isEpMethodChange [

	^ false
]

{ #category : #testing }
EpEvent >> isEpProtocolChange [

	^ false
]

{ #category : #testing }
EpEvent >> isEpRefactoring [

	^ false
]

{ #category : #testing }
EpEvent >> isEpTestResultSet [

	^ false
]

{ #category : #testing }
EpEvent >> isEpTraitChange [

	^ false
]

{ #category : #testing }
EpEvent >> isMonticelloVersionSave [

	^ false
]

{ #category : #testing }
EpEvent >> isMonticelloVersionsLoad [

	^ false
]
