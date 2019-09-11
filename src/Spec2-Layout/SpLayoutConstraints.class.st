Class {
	#name : #SpLayoutConstraints,
	#superclass : #Object,
	#instVars : [
		'spec'
	],
	#category : #'Spec2-Layout-Base'
}

{ #category : #initialization }
SpLayoutConstraints >> initialize [ 

	super initialize.
	self spec: #defaultSpec
]

{ #category : #accessing }
SpLayoutConstraints >> spec [
	^ spec
]

{ #category : #accessing }
SpLayoutConstraints >> spec: anObject [
	spec := anObject
]
