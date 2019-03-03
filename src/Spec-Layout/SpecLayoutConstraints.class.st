Class {
	#name : #SpecLayoutConstraints,
	#superclass : #Object,
	#instVars : [
		'spec'
	],
	#category : #'Spec-Layout-Base'
}

{ #category : #initialization }
SpecLayoutConstraints >> initialize [ 

	super initialize.
	self spec: #defaultSpec
]

{ #category : #accessing }
SpecLayoutConstraints >> spec [
	^ spec
]

{ #category : #accessing }
SpecLayoutConstraints >> spec: anObject [
	spec := anObject
]
