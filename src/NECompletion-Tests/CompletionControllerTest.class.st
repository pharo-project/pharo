"
A NECControllerTest is a test class for testing the behavior of NECController
"
Class {
	#name : #CompletionControllerTest,
	#superclass : #TestCase,
	#instVars : [
		'controller',
		'editor'
	],
	#category : #'NECompletion-Tests'
}

{ #category : #'tests-keyboard' }
CompletionControllerTest >> allSelfClosingCharacters [
	^ {'{}' . '[]' . '()' . '<>' . '||' . '""' . ''''''}
]

{ #category : #'tests-keyboard' }
CompletionControllerTest >> allSmartCharacters [
	^ self allSelfClosingCharacters , { '||'. '<>' }
]

{ #category : #'tests-keyboard' }
CompletionControllerTest >> keyboardEventFor: char [

	| event |
	event := KeyboardEvent new.
	event 
		setType: #keystroke
		buttons: nil
		position:  0@0
		keyValue: char asciiValue 
		charCode: char asciiValue
		hand: nil 
		stamp: Time now.
		
	^event
]

{ #category : #'tests-keyboard' }
CompletionControllerTest >> selectAll [
	editor selectAll 
]

{ #category : #'tests-keyboard' }
CompletionControllerTest >> selectAt: anIndex [
	editor selectFrom: anIndex to: anIndex - 1
]

{ #category : #'tests-keyboard' }
CompletionControllerTest >> selectFrom: start to: stop [
	editor selectFrom: start to: stop
]

{ #category : #'tests-keyboard' }
CompletionControllerTest >> setEditorText: aString [
	editor selectAll; addString:  aString 
	
]

{ #category : #running }
CompletionControllerTest >> setUp [
	super setUp.
	
	editor := RubTextEditor forTextArea: RubTextFieldArea new.
	controller := CompletionEngine new.
	controller setEditor: editor
]

{ #category : #running }
CompletionControllerTest >> tearDown [
	controller := nil.
	editor := nil.
	super tearDown
]

{ #category : #'tests-keyboard' }
CompletionControllerTest >> testSmartBackspace [
	"Pressing backspace inside paired smart characters should remove both of them"
	self allSmartCharacters
		do: [ :chars | 
			self
				setEditorText: '.' , chars , ';';
				selectAt: 3.
			self
				assert: controller smartBackspace
				description: 'smartbackspace if next to smart characters [' , chars , ']'.
			self assert: editor text equals: '.;' ]
]

{ #category : #'tests-keyboard' }
CompletionControllerTest >> testSmartBackspaceOutsideSmartCharacters [

	self setEditorText: '.[];'; selectAll.
	self selectAt: 2.
	self
		assert: controller smartBackspace not
		description: 'smartbackspace ignored if before smart character'.
		
	self selectAt: 4.
	self
		assert: controller smartBackspace not
		description: 'smartbackspace ignored if after smart character'
		
]

{ #category : #'tests-keyboard' }
CompletionControllerTest >> testSmartBackspaceWithSelection [

	self setEditorText: 'Hello'; selectAll.
	self assert: controller smartBackspace not description: 'No smartbackspace if selection'.
	
	self selectAt: 2.
	self
		assert: controller smartBackspace not
		description: 'No smartbackspace if no selection and not a smart character'.
		
]

{ #category : #'tests-keyboard' }
CompletionControllerTest >> testSmartCharacterPairForIfAbsent [

	"Check leading smart character has matching trailing character - e.g. { and }"
	self allSmartCharacters do: [ :pairs |
		self assert: (controller smartCharacterPairFor: pairs first ifAbsent: [ nil ]) equals: pairs first -> pairs last].
	
	"Check trailing smart character has matching leading character - e.g. } and {"
	self allSmartCharacters do: [ :pairs |
		self assert: (controller smartCharacterPairFor: pairs last ifAbsent: [ nil ]) equals: pairs first -> pairs last].
	
	self assert: (controller smartCharacterPairFor: $% ifAbsent: [ nil ]) equals: nil.
		
]

{ #category : #'tests-keyboard' }
CompletionControllerTest >> testSmartCharacterWithEventAndSelection [
	"Test we surround selected text with smart characters properly"
	self
		setEditorText: ' text ';
		selectFrom: 2 to: 5.
		
	controller smartCharacterWithEvent: (self keyboardEventFor: $|).
	self assert: editor text equals: ' |text| '.
	
	self
		setEditorText: ' text ';
		selectFrom: 2 to: 5.
		
	controller smartCharacterWithEvent: (self keyboardEventFor: $[).
	self assert: editor text equals: ' [text] '.
	
	self
		setEditorText: ' text ';
		selectFrom: 2 to: 5.
	controller smartCharacterWithEvent: (self keyboardEventFor: $]).
	self assert: editor text equals: ' [text] '
]

{ #category : #'tests-keyboard' }
CompletionControllerTest >> testSmartCharacterWithEventSelfClosing [
	self
		setEditorText: ' ';
		selectAt: 2.
		
	controller smartCharacterWithEvent: (self keyboardEventFor: $().
	self assert: editor text equals: ' ()'.
	
	self
		setEditorText: ' ';
		selectAt: 2.
		
	controller smartCharacterWithEvent: (self keyboardEventFor: $)).
	self assert: editor text equals: ' '.
	
	self
		setEditorText: ' ';
		selectAt: 2.
		
	controller smartCharacterWithEvent: (self keyboardEventFor: $<).
	self assert: editor text equals: ' '.
	
]

{ #category : #'tests-keyboard' }
CompletionControllerTest >> testSmartCharactersOppositeOf [
	
	self allSmartCharacters
		do: [ :pairs | 
			self
				assert: (controller smartCharacterOppositeOf: pairs first ifAbsent: [ nil ])
				equals: pairs last ]
]

{ #category : #'tests-keyboard' }
CompletionControllerTest >> testSmartCharactersShouldClose [
	"Some smart characters shouldn't auto close, ohers not"
	self assert: (controller smartCharacterShouldClose: $() equals: true.
	self assert: (controller smartCharacterShouldClose: $<) equals: false.
	
]

{ #category : #'tests-keyboard' }
CompletionControllerTest >> testSmartNeedExtraRemoveInForOppositeAt [

	self assert: (controller smartNeedExtraRemoveIn: '()' for: $( opposite:$) at:1).
	self deny: (controller smartNeedExtraRemoveIn: '(()' for: $( opposite:$) at:2).
	self assert: (controller smartNeedExtraRemoveIn: '(1()' for: $( opposite:$) at:3).
	self assert: (controller smartNeedExtraRemoveIn: '(1(1())' for: $( opposite:$) at:5).
	self assert: (controller smartNeedExtraRemoveIn: '((1)1())))' for: $( opposite:$) at:6).
	self deny: (controller smartNeedExtraRemoveIn: '()(()' for: $( opposite:$) at:3).
	self assert: (controller smartNeedExtraRemoveIn: '(foobar()' for: $( opposite:$) at:8).
]
