"
A doIt action on a text. When the text is clicked the text is seen as source code and evaluated (Not really true as both 
the portion of text and its DoIt attribute need to hold the same string)

| stream |
stream := TextStream on: (Text new: 100).
stream 
	withAttribute: (TextDoIt evalString: 'Smalltalk tools browser open')
	do: [ stream nextPutAll: 'Some action...' ].
TextMorph new 
	newContents: stream contents;
	openInWindow .


"
Class {
	#name : #TextDoIt,
	#superclass : #TextAction,
	#instVars : [
		'evalString'
	],
	#category : #'Text-Core-Attributes'
}

{ #category : #'instance creation' }
TextDoIt class >> evalString: str [
	^ self new evalString: str
]

{ #category : #'instance creation' }
TextDoIt class >> scanFrom: strm [
	"read a doit in the funny format used by Text styles on files. d10 factorial;;  end with two semicolons"

	| pos end doit |
	pos := strm position.
	[strm skipTo: $;. strm peek == $;] whileFalse.
	end := strm position - 1.
	strm position: pos.
	doit := strm next: end-pos.
	strm skip: 2.  ";;"
	^ self evalString: doit
]

{ #category : #testing }
TextDoIt >> analyze: aString [

	| list |
	list := super analyze: aString.
	evalString := list at: 1.
	^ list at: 2
]

{ #category : #accessing }
TextDoIt >> evalString: str [
	evalString := str 
]

{ #category : #accessing }
TextDoIt >> info [
	^ evalString
]
