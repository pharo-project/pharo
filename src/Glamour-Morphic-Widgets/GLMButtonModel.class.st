Class {
	#name : #GLMButtonModel,
	#superclass : #GLMMorphicModel,
	#instVars : [
		'glamourAction'
	],
	#category : #'Glamour-Morphic-Widgets'
}

{ #category : #actions }
GLMButtonModel >> buttonLabel [
	^ self glamourAction title
]

{ #category : #actions }
GLMButtonModel >> execute [
	self glamourAction actOn: self glamourPresentation
]

{ #category : #accessing }
GLMButtonModel >> glamourAction [
	^ glamourAction
]

{ #category : #accessing }
GLMButtonModel >> glamourAction: anObject [
	glamourAction := anObject
]
