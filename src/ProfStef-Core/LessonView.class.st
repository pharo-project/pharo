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
	#category : 'ProfStef-Core-Base',
	#package : 'ProfStef-Core',
	#tag : 'Base'
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
	^ StPlaygroundPage new.
]

{ #category : 'gui' }
LessonView >> buildWindow [
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
	^ self window isDisplayed.
]

{ #category : 'gui' }
LessonView >> open [
	window open

]

{ #category : 'gui' }
LessonView >> showLesson: aLesson withTitle: aString [
	self window firstPage windowTitle: aString; contents: aLesson lesson.
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
