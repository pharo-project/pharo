"
I can navigate through an AbstractTutorial subclass (which defines lessons). With #next and #previous, you can go forward and backward through the tutorial. 
"
Class {
	#name : #TutorialPlayer,
	#superclass : #Object,
	#instVars : [
		'tutorialPosition',
		'tutorial'
	],
	#category : #'ProfStef-Core'
}

{ #category : #accessing }
TutorialPlayer >> currentLesson [
	^ self tutorial lessonAt: self tutorialPosition
]

{ #category : #navigating }
TutorialPlayer >> first [
	"Go to the first lesson of the current tutorial and return it."
	self rewind.
	^ self currentLesson
]

{ #category : #navigating }
TutorialPlayer >> last [
	"Go to the last lesson of the current tutorial and return it."
		
	tutorialPosition := self size.
	^ self currentLesson
]

{ #category : #navigating }
TutorialPlayer >> next [
	self tutorialPosition < self size
		ifTrue: [ tutorialPosition := tutorialPosition + 1 ].
	^ self currentLesson
]

{ #category : #navigating }
TutorialPlayer >> previous [
	tutorialPosition >  1 ifTrue: [ tutorialPosition := tutorialPosition  - 1 ].
	^ self currentLesson
]

{ #category : #navigating }
TutorialPlayer >> rewind [
	tutorialPosition := 1
]

{ #category : #accessing }
TutorialPlayer >> size [
	^ self tutorial size
]

{ #category : #accessing }
TutorialPlayer >> tutorial [
	^ tutorial  ifNil: [ tutorial := PharoSyntaxTutorial new ]
]

{ #category : #accessing }
TutorialPlayer >> tutorial: aTutorialInstance [
	"Set on which tutorial instance the receiver is working."
	tutorial := aTutorialInstance
]

{ #category : #accessing }
TutorialPlayer >> tutorialPosition [
	^ tutorialPosition
		ifNil: [ 
			self rewind.
			tutorialPosition ]
]

{ #category : #accessing }
TutorialPlayer >> tutorialPosition: aTutorialPosition [ 

	tutorialPosition := aTutorialPosition 
	
]
