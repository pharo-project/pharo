"
I handle one line of text.
See TextPresenter

self example

You can also have a look at DynamicalPopup for another example.

I handle non source code text, that's why editing shortcuts are not supported (cmd+p just print a p by example).

I provide the following variables and their accessors
- acceptOnCR is a boolean representing if the key cr trigger an accept action,  it is true by default and must be set before built the widget. (if false it will print a cr).
- encrypted is a boolean representing if the text appear like stars (it is only an appearance), beDecrypted and beEncrypted are shortcut to set it.
- entryCompletion is the object used to suggest text while typing, removeEntryCompletion is a shortcut to set it nil.
- placeholder is the ghost text to display in the text zone.

I provide getters of ValueHolder of my super class actionToPerformHolder and textHolder .


todo
- specializing accept:
- specializing eventKeyStrokesForNextFocus , eventKeyStrokesForPreviousFocus
- globalsEntryCompletion
- specializing isCodeCompletionAllowed

"
Class {
	#name : #TextInputFieldPresenter,
	#superclass : #AbstractTextPresenter,
	#instVars : [
		'#acceptOnCR',
		'#entryCompletion',
		'#isPassword => SpecObservableSlot',
		'#placeholder => SpecObservableSlot',
		'#maxLength => SpecObservableSlot'
	],
	#category : #'Spec-Core-Widgets'
}

{ #category : #specs }
TextInputFieldPresenter class >> adapterName [

	^ #TextInputFieldAdapter
]

{ #category : #morphic }
TextInputFieldPresenter >> accept: aText [

	textHolder value: aText.
	actionToPerformHolder value cull: aText.
	
	"Dummy changed to raise an announcement"
	actionPerformedHolder value: aText.
	self changed: #clearUserEdits.
]

{ #category : #api }
TextInputFieldPresenter >> acceptOnCR [
	"<api: #inspect>"
	"Return true if the text is accepted when the Enter key is stroked"

	^ acceptOnCR value
]

{ #category : #api }
TextInputFieldPresenter >> acceptOnCR: aBoolean [
	"<api: #boolean getter: #acceptOnCR registration: #whenAcceptOnCRChanged:>"
	"Set if the text is accepted when the Enter key is stroked or not"

	acceptOnCR value: aBoolean
]

{ #category : #accessing }
TextInputFieldPresenter >> actionToPerformHolder [

	^ actionToPerformHolder
]

{ #category : #api }
TextInputFieldPresenter >> bePassword [
	
	self bePassword: true
]

{ #category : #api }
TextInputFieldPresenter >> bePassword: aBoolean [
	
	isPassword := aBoolean
]

{ #category : #api }
TextInputFieldPresenter >> beText [
	self bePassword: false
]

{ #category : #api }
TextInputFieldPresenter >> entryCompletion [
	"<api: #st getter: #entryCompletion registration: #whenEntryCompletionChanged:>"
	"Return an entry completion used to suggest text while typing"

	^ entryCompletion value
]

{ #category : #api }
TextInputFieldPresenter >> entryCompletion: anEntryCompletion [
	"<api: #st getter: #entryCompletion registration: #whenEntryCompletionChanged:>"
	"Set an entry completion used to suggest text while typing"

	entryCompletion value: anEntryCompletion.
]

{ #category : #focus }
TextInputFieldPresenter >> eventKeyStrokesForNextFocus [
	"String describing the keystroke to perform to jump to the next widget"	
		
	^ { Character tab asKeyCombination }
]

{ #category : #focus }
TextInputFieldPresenter >> eventKeyStrokesForPreviousFocus [
	"String describing the keystroke to perform to jump to the previous widget"
	
	^ { Character tab shift asKeyCombination }
]

{ #category : #initialization }
TextInputFieldPresenter >> initialize [

	super initialize.
	
	maxLength := 0.
	placeholder := ''.
	acceptOnCR := true asValueHolder.
	entryCompletion := nil asValueHolder.
	isPassword := false.
	acceptOnCR whenChangedDo: [ :bool | self changed: #acceptOnCR: with: { bool } ]
]

{ #category : #testing }
TextInputFieldPresenter >> isPassword [
	
	^ isPassword
]

{ #category : #accessing }
TextInputFieldPresenter >> maxLength [
	^ maxLength
]

{ #category : #accessing }
TextInputFieldPresenter >> maxLength: anInteger [ 
	
	maxLength := anInteger.
	self updateText.
]

{ #category : #api }
TextInputFieldPresenter >> placeholder [

	^ placeholder
]

{ #category : #accessing }
TextInputFieldPresenter >> placeholder: aText [

	placeholder := aText
]

{ #category : #api }
TextInputFieldPresenter >> removeEntryCompletion [
	"< api:#do>"
	"Remove the entry completion"
	
	self entryCompletion: nil
]

{ #category : #accessing }
TextInputFieldPresenter >> text: aText [

	| truncatedText |
	truncatedText := self maxLength = 0
		ifTrue: [ aText ]
		ifFalse: [ aText first: (self maxLength min: aText size) ].
	super text: truncatedText
]

{ #category : #accessing }
TextInputFieldPresenter >> textHolder [

	^ textHolder
]

{ #category : #accessing }
TextInputFieldPresenter >> updateText [

	"Update text but applying lenght constraints"
	self text: self text.
]

{ #category : #'api-events' }
TextInputFieldPresenter >> whenAcceptOnCRChangedDo: aBlock [
	"aBlock will be performed when acceptOnCR changed"
	
	acceptOnCR whenChangedDo: aBlock
]

{ #category : #'api-events' }
TextInputFieldPresenter >> whenEntryCompletionChangedDo: aBlock [
	"aBlock will be performed when entryCompletion changed"
	
	entryCompletion whenChangedDo: aBlock
]

{ #category : #'api-events' }
TextInputFieldPresenter >> whenMaxLengthChangedDo: aBlockClosure [ 
	
	self property: #maxLength whenChangedDo: aBlockClosure
]

{ #category : #'api-events' }
TextInputFieldPresenter >> whenPasswordChangedDo: aBlockClosure [ 
	
	self property: #isPassword whenChangedDo: aBlockClosure
]

{ #category : #'api-events' }
TextInputFieldPresenter >> whenPlaceholderChangedDo: aBlock [
	
	self property: #placeholder whenChangedDo: aBlock
]
