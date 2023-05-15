"
This announcement is emitted when we add a method to a class or a trait using:
	=> Behavior >> compile: or TraitBehavior >> compile:
"
Class {
	#name : #MethodAdded,
	#superclass : #MethodAnnouncement,
	#category : #'System-Announcements-System-Methods'
}

{ #category : #'instance creation' }
MethodAdded class >> method: aCompiledMethod [
	^self new
		method: aCompiledMethod;
		yourself
]

{ #category : #accessing }
MethodAdded >> methodAdded [
	^ method
]

{ #category : #accessing }
MethodAdded >> protocol [
	"This should just be `method protocol` when #protocol will return an actual protocol."

	^ method methodClass protocolOfSelector: method selector
]
