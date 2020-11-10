Class {
	#name : #SpGridAxisConstraints,
	#superclass : #Object,
	#traits : 'SpTAlignable',
	#classTraits : 'SpTAlignable classTrait',
	#instVars : [
		'expand'
	],
	#category : #'Spec2-Layout-Grid'
}

{ #category : #accessing }
SpGridAxisConstraints >> beExpand [ 

	self expand: true
]

{ #category : #accessing }
SpGridAxisConstraints >> beNotExpand [ 

	self expand: false
]

{ #category : #private }
SpGridAxisConstraints >> expand [
	^ expand
]

{ #category : #private }
SpGridAxisConstraints >> expand: anObject [
	expand := anObject
]

{ #category : #testing }
SpGridAxisConstraints >> hasAlignment [

	^ self hasVAlign or: [ self hasHAlign ]
]

{ #category : #testing }
SpGridAxisConstraints >> isExpand [

	^ expand == true
]
