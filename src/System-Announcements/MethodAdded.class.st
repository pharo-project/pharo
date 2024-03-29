"
This announcement is emitted when we add a method to a class or a trait using:
	=> Behavior >> compile:
"
Class {
	#name : 'MethodAdded',
	#superclass : 'MethodAnnouncement',
	#category : 'System-Announcements-System-Methods',
	#package : 'System-Announcements',
	#tag : 'System-Methods'
}

{ #category : 'instance creation' }
MethodAdded class >> method: aCompiledMethod [
	^self new
		method: aCompiledMethod;
		yourself
]

{ #category : 'accessing' }
MethodAdded >> methodAdded [
	^ method
]
