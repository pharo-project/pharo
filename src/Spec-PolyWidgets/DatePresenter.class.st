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
	#name : #DatePresenter,
	#superclass : #ComposablePresenter,
	#instVars : [
		'dateLabel',
		'displayBlockHolder',
		'chooseDateButton',
		'dateModel',
		'date'
	],
	#category : #'Spec-PolyWidgets-Widgets'
}

{ #category : #example }
DatePresenter class >> example [
	^ self new openDialogWithSpec
]

{ #category : #example }
DatePresenter class >> exampleModal [
	^ self new openDialogWithSpec modalRelativeTo: self currentWorld
]

{ #category : #accessing }
DatePresenter class >> spec [
	<spec: #default>

	^ SpecLayout composed
			newRow: [ :row | 
				row 
					add: #dateLabel width: 60; 
					"This is to address a not yet fixed Spec issue"
					newRow: [:ugly | 
						ugly
							add: #dateModel;  
							add: #chooseDateButton width: 50 ]] height: 25
			yourself.
]

{ #category : #private }
DatePresenter >> chooseDate [

	| calendar defaultDate |
	defaultDate := self date ifNil: [ self defaultDate ].
	calendar := CalendarMorph openOn: defaultDate.
	calendar
		center: calendar cursorPoint;
		fitInWorld;
		onChoiceSend: #onDateChosen: to: self.
]

{ #category : #'accessing-widgets' }
DatePresenter >> chooseDateButton [

	^ chooseDateButton.
]

{ #category : #accessing }
DatePresenter >> date [

	^ date
]

{ #category : #accessing }
DatePresenter >> date: aDate [

	| dateString |
	date := aDate.
	dateString := self displayBlock value: date.
	self dateModel text: dateString.
]

{ #category : #'accessing-widgets' }
DatePresenter >> dateLabel [

	^ dateLabel.
]

{ #category : #'accessing-widgets' }
DatePresenter >> dateModel [

	^ dateModel.
]

{ #category : #defaults }
DatePresenter >> defaultDate [
	^ Date today 
]

{ #category : #api }
DatePresenter >> displayBlock [
	"<api: #inspect>"
	"Return the one argument block used to wrap your domain specific items.
	The block should return something that can be displayed in a list - like a String or a Text"
	
	^ displayBlockHolder value
]

{ #category : #api }
DatePresenter >> displayBlock: aBlock [
	"<api: #block getter:#displayBlock registration: #whenDisplayBlockChanged:>"
	"Set the one argument block used to transfrom your date into a string"
	
	displayBlockHolder value: aBlock
]

{ #category : #private }
DatePresenter >> iconMorph [

	^ ImageMorph new form: (self iconNamed: #calendarIcon)
]

{ #category : #initialization }
DatePresenter >> initialize [

	super initialize.
	displayBlockHolder := [ :object | object ifNotNil: [ object asStringOrText ] ifNil: '' ] asValueHolder.
	displayBlockHolder whenChangedDo: [ self date: self date ].
	self date: self defaultDate.
]

{ #category : #initialization }
DatePresenter >> initializeWidgets [

	dateLabel := self newLabel.
	dateModel := self newTextInput.
	chooseDateButton := self newButton.
	
	dateLabel
		label: 'Date'.
		
	dateModel
		isCodeCompletionAllowed: false.
	
	chooseDateButton
		icon: self iconMorph;
		action: [ self chooseDate ];
		extent: self iconMorph extent.
]

{ #category : #private }
DatePresenter >> onDateChosen: aChoseDate [

	aChoseDate calendar delete.
	self date: aChoseDate date
]

{ #category : #'api-events' }
DatePresenter >> whenDateChanged: aBlock [

	dateModel whenTextChanged: [ :newText :oldText :announcement :anAnnouncer |
		| oldDate |
		oldDate := date.
		date := newText
			ifNotEmpty: [ Date readFrom: newText readStream ]
			ifEmpty: [ nil ].
		aBlock cull: date cull: oldDate cull: announcement cull: anAnnouncer ]
]
