Class {
	#name : #TextEditorTest,
	#superclass : #TestCase,
	#category : #'Text-Edition-Tests'
}

{ #category : #'private - accessing' }
TextEditorTest >> keyboardEvent [ 
	| event |
	event := KeyboardEvent new.
	event 
		setType: #keystroke
		buttons: 0
		position:  0@0
		keyValue: 0
		charCode: 0
		hand: nil 
		stamp: Time now.
	^ event 	
]

{ #category : #accessing }
TextEditorTest >> testCursorEnd [
	| editor |	 
	editor := TextMorph new editor.
	editor cursorEnd: self keyboardEvent
]

{ #category : #accessing }
TextEditorTest >> testSetSearchString [
	| editor | 
	editor := TextMorph new editor.
	editor setSearchString
]
