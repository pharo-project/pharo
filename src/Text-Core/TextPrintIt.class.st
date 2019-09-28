"
A ""Print It"" action on a text. When the text is clicked the text is seen as source code and evaluated (see comment on parent). After that the result is printed in pop up message.

| stream |
stream := TextStream on: (Text new: 100).
stream 
	withAttribute: (TextPrintIt evalString: '-1 arcCos')
	do: [ stream nextPutAll: '-1 arcCos' ].
TextMorph new 
	newContents: stream contents;
	openInWindow.
"
Class {
	#name : #TextPrintIt,
	#superclass : #TextDoIt,
	#category : #'Text-Core-Attributes'
}
