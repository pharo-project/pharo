"
I am a simple interface to the Display screen canvas to draw text.
"
Class {
	#name : #EDDisplayInterface,
	#superclass : #Object,
	#instVars : [
		'frame',
		'methodFrame'
	],
	#category : #'EmergencyDebugger-Core'
}

{ #category : #'display - properties' }
EDDisplayInterface >> black [
	^ Color black
]

{ #category : #'display - API' }
EDDisplayInterface >> clear [
	self displayCanvas
		frameAndFillRectangle: self frame
		fillColor: self white
		borderWidth: 2
		borderColor: self red.
"	self displayCanvas
		frameAndFillRectangle: self methodFrame
		fillColor: self white
		borderWidth: 2
		borderColor: self red"
]

{ #category : #'text - private' }
EDDisplayInterface >> composeParagraphWith: aText inFrame: aRect [
	| paragraph |
	paragraph := Paragraph new.
	paragraph
		compose: aText
		style: self textStyle
		from: 1
		in: ((aRect insetBy: 4) withHeight: aRect height).
	^ paragraph
]

{ #category : #'display - private' }
EDDisplayInterface >> display: aParagraph in: aRect [
	self displayCanvas
		paragraph: aParagraph
		bounds: aRect
		color: self black
]

{ #category : #'display - properties' }
EDDisplayInterface >> displayCanvas [
	^Display getCanvas
]

{ #category : #'display - properties' }
EDDisplayInterface >> frame [
	^ frame
		ifNil: [ frame := 0 @ 0 corner: DisplayScreen actualScreenSize * (0.3 @ 1) ]
]

{ #category : #'display - properties' }
EDDisplayInterface >> gray [
	^ Color gray
]

{ #category : #'display - properties' }
EDDisplayInterface >> methodFrame [
	^ methodFrame
		ifNil: [ methodFrame := self frame corner corner: DisplayScreen actualScreenSize ]
]

{ #category : #'display - properties' }
EDDisplayInterface >> red [
	^ Color red
]

{ #category : #'display - API' }
EDDisplayInterface >> show: aText [
	self
		display: (self composeParagraphWith: aText inFrame: self frame)
		in: self frame.
	"self
		display: (self composeParagraphWith: aText inFrame: self frame)
		in: self methodFrame."
]

{ #category : #'text - private' }
EDDisplayInterface >> textStyle [
	^ TextStyle default
]

{ #category : #'display - properties' }
EDDisplayInterface >> white [
	^ Color white
]
