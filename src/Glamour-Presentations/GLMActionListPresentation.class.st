"
The goal of a GLMActionListPresentation is to group together and display a set of actions.  Actions should be added using the normal API from GLMPresentation.

The rendred will decide the actual way in which the actions are displayed (usually using a set of buttons).

For an example see GLMBasicExamples>>#simpleActionList
"
Class {
	#name : #GLMActionListPresentation,
	#superclass : #GLMFormatedPresentation,
	#instVars : [
		'isHorizontal'
	],
	#category : #Glamour-Presentations
}

{ #category : #scripting }
GLMActionListPresentation >> beVertical [
	self isHorizontal: false
]

{ #category : #testing }
GLMActionListPresentation >> hasActions [
	^false
]

{ #category : #accessing }
GLMActionListPresentation >> isHorizontal [
	^ isHorizontal ifNil: [isHorizontal := true]
]

{ #category : #accessing }
GLMActionListPresentation >> isHorizontal: anObject [
	isHorizontal := anObject
]

{ #category : #accessing }
GLMActionListPresentation >> isVertical [
	^ self isHorizontal not
]

{ #category : #rendering }
GLMActionListPresentation >> renderGlamorouslyOn: aRenderer [
	self registerAnnouncements.
	^ aRenderer renderActionListPresentation: self
]
