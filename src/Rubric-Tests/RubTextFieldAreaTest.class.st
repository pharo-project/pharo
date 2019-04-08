Class {
	#name : #RubTextFieldAreaTest,
	#superclass : #TestCase,
	#category : #'Rubric-Tests-Editing-Core'
}

{ #category : #utilities }
RubTextFieldAreaTest >> simulatedKeyStrokeFromChar: aCharacter [

	^ self simulatedKeyStrokeFromChar: aCharacter alt: false ctrl: false command: false shift: false
]

{ #category : #utilities }
RubTextFieldAreaTest >> simulatedKeyStrokeFromChar: aCharacter alt: useAlt ctrl: useCtrl command: useCommand shift: useShift [
	| event modifier code |
	event := KeyboardEvent new.
	modifier := 0.
	useShift
		ifTrue: [ modifier := modifier + 8 ].
	useCtrl
		ifTrue: [ modifier := modifier + 16 ].
	useAlt
		ifTrue: [ modifier := modifier + 32 ].
	useCommand
		ifTrue: [ modifier := modifier + 64 ].
	code := aCharacter asInteger.
	event
		setType: #keystroke
		buttons: modifier
		position: 0 @ 0
		keyValue: code
		charCode: code
		hand: World currentHand 
		stamp: Time now.
	^ event
]

{ #category : #tests }
RubTextFieldAreaTest >> testKeyStrokeBackSpaceWithMaxLength [
	| m |
	m := RubTextFieldArea new
		width: 300;
		maxLength: 2;
		yourself.
	self assert: m string size equals: 0.
	m handleEvent: (self simulatedKeyStrokeFromChar: $a).
	self assert: m string  equals: 'a'.
	m handleEvent: (self simulatedKeyStrokeFromChar: $b).
	self assert: m string  equals: 'ab'.
	m handleEvent: (self simulatedKeyStrokeFromChar: Character backspace).
	self assert: m string  equals: 'a'.
	m handleEvent: (self simulatedKeyStrokeFromChar: $c).
	self assert: m string  equals: 'ac'.
]

{ #category : #tests }
RubTextFieldAreaTest >> testKeyStrokeWithMaxLength [
	| m |
	m := RubTextFieldArea new
		width: 300;
		maxLength: 0;
		yourself.
	self assert: m string size isZero.
	m handleEvent: (self simulatedKeyStrokeFromChar: $a).
	self assert: m string size isZero.
	m maxLength: 2.
	m keyStroke: (self simulatedKeyStrokeFromChar: $a).
	self assert: m string size equals: 1.
	m keyStroke: (self simulatedKeyStrokeFromChar: $a).
	self assert: m string size = 2.
	m keyStroke: (self simulatedKeyStrokeFromChar: $a).
	self assert: m string size = 2
]

{ #category : #tests }
RubTextFieldAreaTest >> testMaxLengthIsNegative [
	| m |
	m := RubTextFieldArea new
		width: 300;
		maxLength: -10;
		yourself.
	m setTextWith: '01234567890123456'.
	self assert: m text size equals: 0.

]

{ #category : #tests }
RubTextFieldAreaTest >> testMaxLengthIsSet [
	| m |
	m := RubTextFieldArea new
		width: 300;
		maxLength: 15;
		yourself.
	self assert: m maxLength equals: 15
]

{ #category : #tests }
RubTextFieldAreaTest >> testMaxLengthTruncatesText [
	| m |
	m := RubTextFieldArea new
		width: 300;
		maxLength: 15;
		yourself.
	m setTextWith: '01234567890123456'.
	" the text should be truncated "
	self assert: m text size equals: 15.
	
	" the text is different to ensure that the input is validate and update is sent instead of set "
	m updateTextWith: '7890123456789012345'.
	self assert: m text size equals: 15.
	
	" with a text of size less than maxlength  to check that maxLength has no effect "
	m setTextWith: ''.
	self assert: m text size equals: 0.	
	m setTextWith: 'A'.
	self assert: m text size equals: 1.
	m updateTextWith: ''.
	self assert: m text size equals: 0
]
