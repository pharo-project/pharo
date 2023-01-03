"
Examples rendering text and displaying the result in a Spec Presenter.
"
Class {
	#name : #AthensTextRenderSpecExamples,
	#superclass : #AthensTextRenderExamples,
	#category : #'Athens-Examples-Spec'
}

{ #category : #tests }
AthensTextRenderSpecExamples class >> testSpecWindow [
	<example>
	| presenter window |
	presenter := SpTextPresenter new
		text: self testText;
		yourself.
	window := presenter open.

	presenter withWindowDo: [ :w | w title: 'A fancy looking title text' ].

	^ window window
]

{ #category : #tests }
AthensTextRenderSpecExamples class >> testWindowRender [
	<example>

	| surf win |
	win := self testSpecWindow.
	surf := self surfaceClass extent: win extent.
	win position: 0@0.

	surf drawDuring: [ :canvas |
		canvas pathTransform scaleBy: 1.
		canvas fullDrawMorph: win.
	].

	surf asMorph openInWindow
]
