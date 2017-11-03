"
SUnit tests for class Form
"
Class {
	#name : #FormTest,
	#superclass : #ClassTestCase,
	#category : #Graphics-Tests-Primitives
}

{ #category : #'tests - 32 bit' }
FormTest >> test32BitFormBlackShouldStayBlackAfterSave [
	"Solve bug: https://pharo.fogbugz.com/f/cases/14619/Strange-behavior-of-TestRunner-Icon"

	| blackForm |
	blackForm := (Form extent: 10 @ 10) fillColor: Color black.
	self assert: blackForm colorsUsed first equals: Color black
]

{ #category : #'tests - 32 bit' }
FormTest >> test32BitTranslucentBlackIsBlack [
	|form|
	form := Form extent: 1@1 depth: 32.
	form colorAt: 0@0 put: (Color black alpha: 0.0).
	self assert:  0 equals: (form pixelValueAt: 0@0)
]

{ #category : #tests }
FormTest >> testIsAllWhite [	"self run: #testIsAllWhite"
	"Make sure #isAllWhite works for all bit depths"
	
	#(-32 -16 -8 -4 -2 -1 1 2 4 8 16 32) do:[:d| | form |
		form := Form extent: 16@16 depth: d.
		form fillBlack.
		self deny: form isAllWhite.
		form fillWhite.
		self assert: form isAllWhite.
	].

]
