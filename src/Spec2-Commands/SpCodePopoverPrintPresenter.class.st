"
I show the result of a printing as part of a popover 
"
Class {
	#name : #SpCodePopoverPrintPresenter,
	#superclass : #SpPopoverContentPresenter,
	#instVars : [
		'object',
		'code',
		'text'
	],
	#category : #'Spec2-Commands-Code'
}

{ #category : #accessing }
SpCodePopoverPrintPresenter class >> linesToShow [

	^ 15
]

{ #category : #accessing }
SpCodePopoverPrintPresenter class >> maxLengthShownInPopover [

	^ 80
]

{ #category : #'instance creation' }
SpCodePopoverPrintPresenter class >> newCode: aCodePresenter object: anObject [ 

	^ (self on: anObject)
		code: aCodePresenter;
		yourself 
]

{ #category : #private }
SpCodePopoverPrintPresenter >> basicObjectAsString [
	
	^ [ object printString ]
		on: Error
		do: [ '<error in printString: try ''Inspect it'' to debug>' ]
]

{ #category : #accessing }
SpCodePopoverPrintPresenter >> code [

	^ code
]

{ #category : #accessing }
SpCodePopoverPrintPresenter >> code: aCodePresenter [

	code := aCodePresenter
]

{ #category : #'api-focus' }
SpCodePopoverPrintPresenter >> defaultKeyboardFocus [

	^ text
]

{ #category : #api }
SpCodePopoverPrintPresenter >> dismiss [

	super dismiss.
	self code takeKeyboardFocus
]

{ #category : #actions }
SpCodePopoverPrintPresenter >> dismissAndInspect [

	self dismiss.
	object inspect.
	"(SpCodeInspectItCommand forContext: context) execute"
]

{ #category : #actions }
SpCodePopoverPrintPresenter >> dismissAndPrint [

	self dismiss.
	self code insertAndSelectAfterCurrentSelection: self objectAsPrintString
]

{ #category : #initialization }
SpCodePopoverPrintPresenter >> initializePresenters [

	self initializeText.

	layout := SpBoxLayout newHorizontal
		spacing: 5;
		borderWidth: 5;
		add: text;
		yourself.

	text eventHandler whenFocusLostDo: [ self dismiss ].

	text
		bindKeyCombination: Character cr asKeyCombination | PharoShortcuts current printItShortcut
			toAction: [ self dismissAndPrint ];
		bindKeyCombination: PharoShortcuts current inspectItShortcut 
			toAction: [ self dismissAndInspect ];
		bindKeyCombination: PharoShortcuts current escapeShortcut | PharoShortcuts current backspaceShortcut | PharoShortcuts current cancelShortcut
			toAction: [ self dismiss ]
]

{ #category : #initialization }
SpCodePopoverPrintPresenter >> initializeText [
	| string numLines |

	string := self objectAsString.
	numLines := string lines size.	
	text := self 
		newTextMultiLine: string 
		withScrollBars: numLines >= self class linesToShow.
		
	text 
		"addStyle: 'code';"
		addStyle: 'codePopover'
]

{ #category : #initialization }
SpCodePopoverPrintPresenter >> newTextMultiLine: aString withScrollBars: withScrollBars [
	| newText |		

	newText := self newText
		propagateNaturalWidth: true;
		propagateNaturalHeight: withScrollBars not;
		editable: false;
		text: aString;
		yourself.

	"this is needed just for the gtkbackend"
	withScrollBars ifTrue: [ 
		newText addScrollBarStyle: 'scrollbarPopoverLarge' ].
	
	^ newText
]

{ #category : #initialization }
SpCodePopoverPrintPresenter >> newTextSingleLine [
		
	^ self newTextInput
]

{ #category : #initialization }
SpCodePopoverPrintPresenter >> newTextSingleLine: aString [
		
	^ self newLabel label: aString
]

{ #category : #accessing }
SpCodePopoverPrintPresenter >> objectAsPrintString [
	
	^ String streamContents: [ :stream |
		stream 
			<< ' "'
			<< self basicObjectAsString
			<< '"' ]
]

{ #category : #accessing }
SpCodePopoverPrintPresenter >> objectAsString [
	| string |
	
	string := self basicObjectAsString.
	string size <= self class maxLengthShownInPopover 
		ifTrue: [ ^ string ].
	
	(string includesAny: Character separators) 
		ifTrue: [ ^ string withNoLineLongerThan: self class maxLengthShownInPopover ].
	
	^ String streamContents: [ :stream | 
			| lines |
			lines := self basicObjectAsString 
				groupsOf: self class maxLengthShownInPopover 
				atATimeCollect: [ :group | group ].
			lines
				do: [ :each | stream << each ]
				separatedBy: [ stream cr ] ]
]

{ #category : #'accessing model' }
SpCodePopoverPrintPresenter >> setModelBeforeInitialization: anObject [

	object := anObject
]
