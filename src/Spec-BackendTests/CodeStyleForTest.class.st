Class {
	#name : #CodeStyleForTest,
	#superclass : #Object,
	#instVars : [
		'foreground'
	],
	#category : #'Spec-BackendTests'
}

{ #category : #comparing }
CodeStyleForTest >> = aTag [ 

	^ self foreground = aTag foreground
]

{ #category : #accessing }
CodeStyleForTest >> foreground [
	^ foreground
]

{ #category : #accessing }
CodeStyleForTest >> foreground: anObject [
	foreground := anObject
]
