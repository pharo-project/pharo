Class {
	#name : #RGWeakLayout,
	#superclass : #RGPointerLayout,
	#category : #'Ring-Core-Kernel'
}

{ #category : #'testing types' }
RGWeakLayout >> isVariableLayout [

	^ true
]

{ #category : #'testing types' }
RGWeakLayout >> isWeakLayout [

	^ true
]

{ #category : #accessing }
RGWeakLayout >> layoutName [

	^ #EphemeronLayout
]
