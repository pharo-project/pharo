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
- ghostText is the ghost text to display in the text zone.

I provide getters of ValueHolder of my super class actionToPerformHolder and textHolder .


todo
- specializing accept:
- specializing eventKeyStrokesForNextFocus , eventKeyStrokesForPreviousFocus
- globalsEntryCompletion
- specializing isCodeCompletionAllowed

"
Class {
	#name : #TextInputFieldPresenter,
	#superclass : #TextPresenter,
	#instVars : [
		'ghostText',
		'acceptOnCR',
		'entryCompletion',
		'isEncrypted'
	],
	#category : #'Spec-Core-Widgets'
}

{ #category : #specs }
TextInputFieldPresenter class >> adapterName [

	^ #TextInputFieldAdapter
]

{ #category : #specs }
TextInputFieldPresenter class >> defaultSpec [
	<spec: #default>
	
	^ #(TextInputFieldAdapter
		adapt: #(model))
]

{ #category : #example }
TextInputFieldPresenter class >> example [
	<sampleInstance>
	^ self new
	 extent: 300 @ 25;
	 openWithSpec;
	 ghostText: 'This is a ghost';
	 text: 'This is a text'
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
TextInputFieldPresenter >> beDecrypted [
	"<api: #do>"
	"Decrypt the entered text"
	
	self encrypted: false
]

{ #category : #api }
TextInputFieldPresenter >> beEncrypted [
"	<api: #do>"
	"Encrypt the entered text"
	
	self encrypted: true
]

{ #category : #accessing }
TextInputFieldPresenter >> encrypted [

	^ isEncrypted value
]

{ #category : #accessing }
TextInputFieldPresenter >> encrypted: aBoolean [

	isEncrypted value: aBoolean 
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

{ #category : #api }
TextInputFieldPresenter >> ghostText [
	"<api: #inspect>"
	"Return the ghost text of the text zone"

	^ ghostText value
]

{ #category : #accessing }
TextInputFieldPresenter >> ghostText: aText [
	"<api: #string getter: #ghostText registration: #whenGhostTextChanged:>"
	"Set the ghost text of the text zone"

	ghostText value: aText.
]

{ #category : #initialization }
TextInputFieldPresenter >> initialize [

	super initialize.
	
	ghostText := '' asValueHolder.
	acceptOnCR := true asValueHolder.
	entryCompletion := nil asValueHolder.
	isEncrypted := false asValueHolder.
	
	ghostText  whenChangedDo: [ :txt | self changed: #ghostText: with: { txt } ].
	acceptOnCR whenChangedDo: [ :bool | self changed: #acceptOnCR: with: { bool } ].
	isEncrypted whenChangedDo: [ :bool | self changed: #encrypted: with: { bool } ].
]

{ #category : #api }
TextInputFieldPresenter >> removeEntryCompletion [
	"< api:#do>"
	"Remove the entry completion"
	
	self entryCompletion: nil
]

{ #category : #accessing }
TextInputFieldPresenter >> textHolder [

	^ textHolder
]

{ #category : #'api-events' }
TextInputFieldPresenter >> whenAcceptOnCRChanged: aBlock [
	"aBlock will be performed when acceptOnCR changed"
	
	acceptOnCR whenChangedDo: aBlock
]

{ #category : #'api-events' }
TextInputFieldPresenter >> whenEntryCompletionChanged: aBlock [
	"aBlock will be performed when entryCompletion changed"
	
	entryCompletion whenChangedDo: aBlock
]

{ #category : #'api-events' }
TextInputFieldPresenter >> whenGhostTextChanged: aBlock [
	"aBlock will be performed when ghostText changed"
	
	ghostText whenChangedDo: aBlock
]
