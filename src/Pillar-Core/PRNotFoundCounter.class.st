"
I am a subclass used when a reference is not found.

In this case no number can be returned so a Warning is raised, and a ? is displayed
"
Class {
	#name : #PRNotFoundCounter,
	#superclass : #PRCascadingCounter,
	#category : #'Pillar-Core-Counters'
}

{ #category : #accessing }
PRNotFoundCounter >> elements [

	^ { '¿?' }
]

{ #category : #testing }
PRNotFoundCounter >> isValidCounter [
	^ false
]
