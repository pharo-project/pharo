"
A LessonView displays a Lesson instance.

LessonView new open
"
Class {
	#name : 'LessonView',
	#superclass : 'Object',
	#instVars : [
		'page',
		'window',
		'sourceTextModel'
	],
	#category : 'ProfStef-Core',
	#package : 'ProfStef-Core'
}

{ #category : 'gui' }
LessonView class >> menuOn: aBuilder [
	<LessonViewContextMenu>

	(aBuilder item: #'Do it' translated)
		keyText: 'd';
		selector: #doIt;
		help: nil;
		iconName: #smallDoIt.
	(aBuilder item: #'Print it' translated)
		keyText: 'p';
		selector: #printIt;
		help: nil;
		iconName: #smallPrintIt.
	(aBuilder item: #'Inspect it' translated)
		keyText: 'i';
		selector: #inspectIt;
		help: nil;
		iconName: #smallInspectIt
]

{ #category : 'accessing' }
LessonView >> bindings [

	"dynamic variable binding not needed for Lessons"

	^ Dictionary new
]

{ #category : 'gui' }
LessonView >> buildPage [
	| scrolledText |

	scrolledText := StPlaygroundPage new.
	^ scrolledText
]

{ #category : 'gui' }
LessonView >> buildWindow [
	"^ SystemWindow labelled: 'PrStef lesson'"
	^ StProfStefPlayground new.
]

{ #category : 'gui' }
LessonView >> close [
	window delete
]

{ #category : 'testing' }
LessonView >> hasBindingOf: aString [

 	^ self bindings includesKey: aString
]

{ #category : 'initialization' }
LessonView >> initialize [

	super initialize.
	window := self buildWindow.
	page := self buildPage.
	window loadPage: page.
	

]

{ #category : 'testing' }
LessonView >> isOpenInWindow [
	"^ self currentWorld systemWindows includes: window"
	^ self window isDisplayed.
	"^ self currentWorld currentWindow label == window windowTitle."
]

{ #category : 'gui' }
LessonView >> open [
	"menu: #shoutMorphFillMenu:."
	"window openInWorld."
	window open

]

{ #category : 'gui' }
LessonView >> showLesson: aLesson withTitle: aString [
	"self window setLabel: aString.
	self sourceTextModel setText: aLesson lesson.
	self buildWindow."
	"|page|
	page := (self buildText contents: aLesson lesson) flush."
	self window firstPage windowTitle: aString.
	self window firstPage contents: aLesson lesson.
	self window firstPage text unselectAll.
	self isOpenInWindow ifFalse: [ self open ]
]

{ #category : 'accessing' }
LessonView >> text [
	^ window firstPage contents.
]

{ #category : 'accessing' }
LessonView >> window [
	^ window
]
