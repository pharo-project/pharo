Class {
	#name : #SpWindowWillClose,
	#superclass : #SpWindowAnnouncement,
	#category : #'Spec2-Core-Support'
}

{ #category : #accessing }
SpWindowWillClose >> window [
	^ window
]

{ #category : #accessing }
SpWindowWillClose >> window: anObject [
	window := anObject
]
