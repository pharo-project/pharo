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
	#name : #SpTextInputFieldPresenter,
	#superclass : #SpAbstractTextPresenter,
	#instVars : [
		'#acceptOnCR => SpObservableSlot',
		'#entryCompletion => SpObservableSlot',
		'#isPassword => SpObservableSlot',
		'#maxLength => SpObservableSlot'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #specs }
SpTextInputFieldPresenter class >> adapterName [

	^ #TextInputFieldAdapter
]

{ #category : #morphic }
SpTextInputFieldPresenter >> accept: aText [

	self text: aText.
	self acceptBlock cull: aText.
	
	"Raise an announcement"
	self actionPerformed.
	self changed: #clearUserEdits.
]

{ #category : #api }
SpTextInputFieldPresenter >> acceptOnCR [
	"Return true if the text is accepted when the Enter key is stroked"

	^ acceptOnCR
]

{ #category : #api }
SpTextInputFieldPresenter >> acceptOnCR: aBoolean [
	"Set if the text is accepted when the Enter key is stroked or not"

	acceptOnCR := aBoolean
]

{ #category : #api }
SpTextInputFieldPresenter >> bePassword [
	
	self bePassword: true
]

{ #category : #api }
SpTextInputFieldPresenter >> bePassword: aBoolean [
	
	isPassword := aBoolean
]

{ #category : #api }
SpTextInputFieldPresenter >> beText [
	self bePassword: false
]

{ #category : #api }
SpTextInputFieldPresenter >> entryCompletion [
	"Return an entry completion used to suggest text while typing"

	^ entryCompletion
]

{ #category : #api }
SpTextInputFieldPresenter >> entryCompletion: anEntryCompletion [
	"Set an entry completion used to suggest text while typing"

	entryCompletion := anEntryCompletion
]

{ #category : #focus }
SpTextInputFieldPresenter >> eventKeyStrokesForNextFocus [
	"String describing the keystroke to perform to jump to the next widget"	
		
	^ { Character tab asKeyCombination }
]

{ #category : #focus }
SpTextInputFieldPresenter >> eventKeyStrokesForPreviousFocus [
	"String describing the keystroke to perform to jump to the previous widget"
	
	^ { Character tab shift asKeyCombination }
]

{ #category : #initialization }
SpTextInputFieldPresenter >> initialize [
	super initialize.

	maxLength := 0.
	acceptOnCR := true.
	isPassword := false.
	self whenAcceptOnCRChangedDo: [ :bool | self changed: #acceptOnCR: with: {bool} ]
]

{ #category : #testing }
SpTextInputFieldPresenter >> isPassword [
	
	^ isPassword
]

{ #category : #accessing }
SpTextInputFieldPresenter >> maxLength [
	^ maxLength
]

{ #category : #accessing }
SpTextInputFieldPresenter >> maxLength: anInteger [ 
	
	maxLength := anInteger.
	self updateText.
]

{ #category : #api }
SpTextInputFieldPresenter >> removeEntryCompletion [
	"Remove the entry completion"
	
	self entryCompletion: nil
]

{ #category : #accessing }
SpTextInputFieldPresenter >> text: aText [

	| truncatedText |
	truncatedText := self maxLength = 0
		ifTrue: [ aText ]
		ifFalse: [ aText first: (self maxLength min: aText size) ].
	super text: truncatedText
]

{ #category : #accessing }
SpTextInputFieldPresenter >> updateText [

	"Update text but applying lenght constraints"
	self text: self text.
]

{ #category : #'api-events' }
SpTextInputFieldPresenter >> whenAcceptOnCRChangedDo: aBlock [
	"aBlock will be performed when acceptOnCR changed"

	self property: #acceptOnCR whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpTextInputFieldPresenter >> whenEntryCompletionChangedDo: aBlock [
	"aBlock will be performed when entryCompletion changed"

	self property: #entryCompletion whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpTextInputFieldPresenter >> whenMaxLengthChangedDo: aBlockClosure [ 
	
	self property: #maxLength whenChangedDo: aBlockClosure
]

{ #category : #'api-events' }
SpTextInputFieldPresenter >> whenPasswordChangedDo: aBlockClosure [ 
	
	self property: #isPassword whenChangedDo: aBlockClosure
]
