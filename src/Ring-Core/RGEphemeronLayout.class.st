Class {
	#name : #RGEphemeronLayout,
	#superclass : #RGPointerLayout,
	#category : #'Ring-Core-Kernel'
}

{ #category : #'testing types' }
RGEphemeronLayout >> isEphemeronLayout [

	^ true
]

{ #category : #accessing }
RGEphemeronLayout >> layoutName [

	^ #EphemeronLayout
]
