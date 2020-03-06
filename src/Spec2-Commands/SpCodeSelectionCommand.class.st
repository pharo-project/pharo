"
I'm a code presenter command who's actions depends on the selected text.
In case no text is selected, the commands will take the text from cursor position to begining of line as an ""ad hoc"" selection.

"
Class {
	#name : #SpCodeSelectionCommand,
	#superclass : #SpToolCommand,
	#category : #'Spec2-Commands-Code'
}

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
	selection := context selectedText ifEmpty: [ ^ '' ].
	^ self evaluate: selection andDo: aBlock
]
