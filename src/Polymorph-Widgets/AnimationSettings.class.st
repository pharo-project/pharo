"
Animation settings
"
Class {
	#name : #AnimationSettings,
	#superclass : #Model,
	#instVars : [
		'useAnimation',
		'animateClosing',
		'delay',
		'numberOfSteps'
	],
	#category : #'Polymorph-Widgets-Themes'
}

{ #category : #accessing }
AnimationSettings >> animateClosing [
	^ animateClosing ifNil: [animateClosing := false]
]

{ #category : #accessing }
AnimationSettings >> animateClosing: aBoolean [
	animateClosing := aBoolean
]

{ #category : #accessing }
AnimationSettings >> delay [
	^ delay ifNil: [delay := 8]
]

{ #category : #accessing }
AnimationSettings >> delay: anInteger [
	delay := anInteger
]

{ #category : #accessing }
AnimationSettings >> numberOfSteps [
	^ numberOfSteps ifNil: [numberOfSteps := 20]
]

{ #category : #accessing }
AnimationSettings >> numberOfSteps: anInteger [
	numberOfSteps := anInteger
]

{ #category : #accessing }
AnimationSettings >> useAnimation [
	^ useAnimation ifNil: [ useAnimation := false ]
]

{ #category : #accessing }
AnimationSettings >> useAnimation: aBoolean [
	useAnimation := aBoolean
]
