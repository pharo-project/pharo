"
I wrap a stream and generate documentation in a concrete format.

The intent is that sister classes will implement various concrete formats (HTML, Man page, LaTeX, colored terminal output…)
"
Class {
	#name : #ClapDocumentationFormatter,
	#superclass : #Object,
	#instVars : [
		'stream',
		'newLineString'
	],
	#category : #'Clap-Core-Documentation'
}

{ #category : #'instance creation' }
ClapDocumentationFormatter class >> on: aCharacterWriteStream [
	^ self new
		stream: aCharacterWriteStream;
		yourself
]

{ #category : #accessing }
ClapDocumentationFormatter >> columnIndent [
	^ 16
]

{ #category : #accessing }
ClapDocumentationFormatter >> columnSeparation [
	^ 3
]

{ #category : #accessing }
ClapDocumentationFormatter >> contents [
	^ stream contents
]

{ #category : #flushing }
ClapDocumentationFormatter >> flush [
	stream flush
]

{ #category : #accessing }
ClapDocumentationFormatter >> listIndent [
	^ 4
]

{ #category : #formattting }
ClapDocumentationFormatter >> newLine [
	^ stream newLine
]

{ #category : #formattting }
ClapDocumentationFormatter >> section: titleString with: contentsBlock [
	| contents |
	contents := self class new.
	contentsBlock value: contents.
	
	contents isEmpty ifFalse: [
		stream
			nextPutAll: titleString; nextPut: $:;
			newLine;
			nextPutAll: contents contents ]
]

{ #category : #accessing }
ClapDocumentationFormatter >> space [
	^ stream space
]

{ #category : #formattting }
ClapDocumentationFormatter >> space: anInteger [
	anInteger timesRepeat: [ stream space ]
]

{ #category : #initialization }
ClapDocumentationFormatter >> stream: aStream [
	stream := 	(aStream respondsTo: #newLine)
		ifTrue: [ aStream ]
		ifFalse: [ ZnNewLineWriterStream on: aStream ]
]

{ #category : #formattting }
ClapDocumentationFormatter >> tabularize: associations [
	associations do: [ :each |
		| keyWidth |
		keyWidth := each key size + self listIndent.
		self space: self listIndent.
		stream nextPutAll: each key.
		keyWidth + self columnSeparation > self columnIndent
			ifTrue: [ self newLine; space: self columnIndent ]
			ifFalse: [ self space: self columnIndent - keyWidth ].
		stream nextPutAll: each value.
		self newLine ]
]

{ #category : #formattting }
ClapDocumentationFormatter >> text: aString [
	stream nextPutAll: aString
]
