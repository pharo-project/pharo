"
I'm a code presenter command who's actions depends on the selected text.
In case no text is selected, the commands will take the text from cursor position to begining of line as an ""ad hoc"" selection.

"
Class {
	#name : #SpCodeSelectionCommand,
	#superclass : #SpCodeCommand,
	#category : #'Spec2-Commands-Code'
}

{ #category : #private }
SpCodeSelectionCommand >> afterSelectionInsertAndSelect: aString [
	| selection |

	selection := self context selection.
	self context 
		insert: aString 
		at: selection last + 1.
	self context selection: (selection last + 1 to: selection last + aString size)
]

{ #category : #private }
SpCodeSelectionCommand >> evaluate: aString andDo: aBlock [
	"Treat the current selection as an expression; evaluate it and invoke 
	 aBlock with the result."
	| stream result receiver evaluationContext |
	
	stream := aString readStream.
	
	receiver := self context doItReceiver.
	evaluationContext := self context doItContext.
	result := receiver class compiler
		source: stream;
		context: evaluationContext;
		receiver: receiver;
		requestor: receiver;
		failBlock:  [ ^ nil ];
		evaluate.		

	^ aBlock value: result
]

{ #category : #private }
SpCodeSelectionCommand >> evaluateSelectionAndDo: aBlock [
	"Treat the current selection as an expression; evaluate it and invoke aBlock with the result.
	If no selection is present select the current line."
	| selection |

	selection := self selectionOrLine.
	selection ifEmpty: [ ^ '' ].
	^ self 
		evaluate: selection 
		andDo: aBlock
]

{ #category : #accessing }
SpCodeSelectionCommand >> selectLine [

	self context selectLine.
	^ self selection
]

{ #category : #accessing }
SpCodeSelectionCommand >> selection [
	| selection |
	self flag: 'I wonder about the naming of the classes: should actions based on selection not be available also on text presenters?'.
	selection := context selection.
	selection ifEmpty: [ ^ '' ].
	self flag: 'The following code has been commented: it seems wrong and provokes the current context to be restarted in the debugger'.
	"context accept."
	^ context text copyFrom: selection first to: selection last
]

{ #category : #accessing }
SpCodeSelectionCommand >> selectionOrLine [
	
	^ self selection ifEmpty: [ self selectLine ]
]
