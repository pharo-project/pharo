"
I'm a Stream of Text objects. 

ex:

	|ts|
	ts := TextStream on: (Text new: 50).
 	ts nextPutAll: 'I ' asText.
	ts nextPutAll: 'love' asText allBold.
 	ts nextPutAll: ' Pharo'.
	TextMorph new 
		newContents: ts contents;
 		openInWindowLabeled: 'TextStream test'
"
Class {
	#name : #TextStream,
	#superclass : #WriteStream,
	#category : #'Text-Core-Base'
}

{ #category : #accessing }
TextStream >> applyAttribute: att beginningAt: startPos [
	collection addAttribute: att from: startPos to: self position
]

{ #category : #accessing }
TextStream >> nextPutAll: aCollection [ 
 	"Optimized access to get around Text at:Put: overhead"
 	| n |
 	n := aCollection size.
      position + n > writeLimit
        ifTrue:
         [self growTo: position + n + 10].
 	collection 
 		replaceFrom: position+1
 		to: position + n
 		with: aCollection
 		startingAt: 1.
 	position := position + n.
	^ aCollection
]

{ #category : #private }
TextStream >> withAttribute: att do: strmBlock [
"Add text to the stream with a block and apply its text attribute"
	| pos1 val |
	pos1 := self position.
	val := strmBlock value.
	collection addAttribute: att from: pos1+1 to: self position.
	^ val
]

{ #category : #private }
TextStream >> withAttributes: attributes do: streamBlock [ 
"Add text to the stream with a block and apply its text attributes (in collection)"
	| pos1 val |
	pos1 := self position.
	val := streamBlock value.
	attributes do: [:attribute |
		collection
			addAttribute: attribute
			from: pos1 + 1
			to: self position].
	^ val
]
