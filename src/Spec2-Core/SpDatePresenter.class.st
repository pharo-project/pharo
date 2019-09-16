"
A DatePresenter is a widget for choosing dates. It consists of:
- A text box, into which you can type any string which can be converted into a Smalltalk date
- A button, which displays a calendar, from which you can select a date

Usage:
- the simplest way is to add a DatePresenter in your UI, and send #date when you want its value.
- or, register to be notified when the date changes:
	DatePresenter new
		date: self date;
		whenDateChanged: [ :newDate | self date: newDate ].

As it is Spec-based, it can be easily adapted and composed into a larger UI.
"
Class {
	#name : #SpDatePresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'#chooseDateButton',
		'#dateModel',
		'#date',
		'#displayBlock => SpObservableSlot'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #specs }
SpDatePresenter class >> defaultSpec [
	^ SpLayout composed
		newRow: [ :row | 
			row
				add: #dateModel;
				add: #chooseDateButton width: 50 ]
			height: 25;
		yourself
]

{ #category : #private }
SpDatePresenter >> chooseDate [

	| calendar defaultDate |
	defaultDate := self date ifNil: [ self defaultDate ].
	calendar := CalendarMorph openOn: defaultDate.
	calendar
		center: calendar cursorPoint;
		fitInWorld;
		onChoiceSend: #onDateChosen: to: self.
]

{ #category : #'accessing-widgets' }
SpDatePresenter >> chooseDateButton [

	^ chooseDateButton.
]

{ #category : #accessing }
SpDatePresenter >> date [

	^ date
]

{ #category : #accessing }
SpDatePresenter >> date: aDate [

	| dateString |
	date := aDate.
	dateString := self display value: date.
	self dateModel text: dateString.
]

{ #category : #'accessing-widgets' }
SpDatePresenter >> dateModel [

	^ dateModel.
]

{ #category : #defaults }
SpDatePresenter >> defaultDate [
	^ Date today 
]

{ #category : #api }
SpDatePresenter >> display [
	"Return the one argument block used to wrap your domain specific items.
	The block should return something that can be displayed in a list - like a String or a Text"

	^ displayBlock
]

{ #category : #api }
SpDatePresenter >> display: aBlock [
	"Set the one argument block used to transfrom your date into a string"

	displayBlock := aBlock
]

{ #category : #api }
SpDatePresenter >> displayBlock [
	"Return the one argument block used to wrap your domain specific items.
	The block should return something that can be displayed in a list - like a String or a Text"

	self
		deprecated: 'Please use #display instead'
		transformWith: '`@receiver displayBlock' 
						-> '`@receiver display'.
	
	^ displayBlock
]

{ #category : #api }
SpDatePresenter >> displayBlock: aBlock [
	"Set the one argument block used to transfrom your date into a string"

	self
		deprecated: 'Please use #display: instead'
		transformWith: '`@receiver displayBlock: `@arg' 
						-> '`@receiver display: `@arg'.

	displayBlock := aBlock
]

{ #category : #private }
SpDatePresenter >> iconMorph [

	^ ImageMorph new form: (self iconNamed: #calendarIcon)
]

{ #category : #initialization }
SpDatePresenter >> initialize [
	super initialize.
	displayBlock := [ :object | object ifNotNil: [ object asStringOrText ] ifNil: '' ].
	self property: #displayBlock whenChangedDo: [ self date: self date ].
	self date: self defaultDate
]

{ #category : #initialization }
SpDatePresenter >> initializePresenters [

	dateModel := self newTextInput.
	chooseDateButton := self newButton.
	
	chooseDateButton
		icon: self iconMorph;
		action: [ self chooseDate ];
		extent: self iconMorph extent.
]

{ #category : #private }
SpDatePresenter >> onDateChosen: aChoseDate [

	aChoseDate calendar delete.
	self date: aChoseDate date
]

{ #category : #'api-events' }
SpDatePresenter >> whenDateChanged: aBlock [

	dateModel whenTextChanged: [ :newText :oldText :announcement :anAnnouncer |
		| oldDate |
		oldDate := date.
		date := newText
			ifNotEmpty: [ Date readFrom: newText readStream ]
			ifEmpty: [ nil ].
		aBlock cull: date cull: oldDate cull: announcement cull: anAnnouncer ]
]
