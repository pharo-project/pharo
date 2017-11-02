"
This announcement is emited when we add a method to a class or a trait using:
	=> Behavior >> compile: or TraitBehavior >> compile:
"
Class {
	#name : #MethodAdded,
	#superclass : #MethodAnnouncement,
	#category : 'System-Announcements-System-Methods'
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
