"
I handle one line of text.
self example

## Transmissions
- #inputTextPort (default)
- #outputTextChangedPort (default)


"
Class {
	#name : #SpTextInputFieldPresenter,
	#superclass : #SpAbstractTextPresenter,
	#instVars : [
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
	isPassword := false
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

{ #category : #'api-events' }
SpTextInputFieldPresenter >> whenSubmitDo: aBlock [
	"This method will add a 'submit' event to the widget. It will react when user presses meta+s 
	 key (this is for historical reasons) and with enter."

	self 
		bindKeyCombination: Character cr asKeyCombination | $s meta 
		toAction: aBlock
]
