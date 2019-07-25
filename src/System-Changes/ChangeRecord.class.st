"
A ChangeRecord represents a change recorded on a file in fileOut format.
It includes a type (more needs to be done here), and additional information
for certain types such as method defs which need class and category.
"
Class {
	#name : #ChangeRecord,
	#superclass : #Object,
	#instVars : [
		'file',
		'position',
		'type',
		'class',
		'category',
		'meta',
		'stamp'
	],
	#category : #'System-Changes-Records'
}

{ #category : #testing }
ChangeRecord >> <= anotherOne [

	self stamp ifNil: [ ^ false ].
	anotherOne stamp ifNil: [ ^ true ].
	^ self timeStamp <= anotherOne timeStamp 
]

{ #category : #access }
ChangeRecord >> category [
	^category
]

{ #category : #testing }
ChangeRecord >> classIncludesSelector [
	
	| aClass | 
	^ (aClass := self methodClass) notNil
			and: [aClass includesSelector: self methodSelector]
]

{ #category : #access }
ChangeRecord >> commentClass [ 
	| commentClass |
	type == #classComment ifFalse: [^ nil].
	(Smalltalk includesKey: class asSymbol) ifFalse: [^ nil].
	commentClass := Smalltalk at: class asSymbol.
	^meta ifTrue: [commentClass class]
		ifFalse: [commentClass]

]

{ #category : #initialization }
ChangeRecord >> file: f position: p type: t [
	file := f.
	position := p.
	type := t
]

{ #category : #initialization }
ChangeRecord >> file: f position: p type: t class: c category: cat meta: m stamp: s [
	self file: f position: p type: t.
	class := c.
	category := cat.
	meta := m.
	stamp := s
]

{ #category : #initialization }
ChangeRecord >> fileIn [
	"File the receiver in.  If I represent a method or a class-comment, file the method in and make a note of it in the recent-submissions list; if I represent a do-it, then, well, do it."

	UIManager default
		showWaitCursorWhile:			
			[ 
			| methodClass s |
			(methodClass := self methodClass) notNil
				ifTrue:
					[ 
					methodClass
						compile: self text
						classified: category
						withStamp: stamp
						notifying: nil ].
			type == #doIt
				ifTrue:
					[ 
					((s := self string) beginsWith: '----')
						ifFalse: [ self class compiler evaluate: s ] ].
			type == #classComment
				ifTrue:
					[ (Smalltalk globals at: class asSymbol) comment: self text stamp: stamp ] ]
]

{ #category : #access }
ChangeRecord >> fileName [
	^file ifNotNil: [ file name ] ifNil: [ '<no file>' ]
]

{ #category : #access }
ChangeRecord >> fileOutOn: aFileStream [
	"File the receiver out on the given file stream"

	| aString |
	type == #method
		ifTrue:
			[aFileStream nextPut: $!.
			aString :=  class asString
							, (meta ifTrue: [' class methodsFor: ']
									ifFalse: [' methodsFor: '])
							, category asString printString.
			stamp ifNotNil:
				[aString := aString, ' stamp: ''', stamp, ''''].
			aFileStream nextChunkPut: aString.
			aFileStream cr].

	type == #preamble ifTrue: [aFileStream nextPut: $!].

	type == #classComment
		ifTrue:
			[aFileStream nextPut: $!.
			aFileStream nextChunkPut: class asString, ' commentStamp: ', stamp storeString.
			aFileStream cr].

	aFileStream nextChunkPut: self string.
	type == #method ifTrue: [aFileStream nextChunkPut: ' '].
	aFileStream cr
]

{ #category : #access }
ChangeRecord >> isMetaClassChange [
	^meta
]

{ #category : #testing }
ChangeRecord >> isMethodDefinedInImage [
	"answer whether the method represented by the receiver is present in the image"
	
	^ self type = #method
		and: [self classIncludesSelector]
]

{ #category : #testing }
ChangeRecord >> isMethodNotDefinedInImage [
	"answer whether the method represented by the receiver is not present in the image.
	pay attention is it not just isMethodDefinedInImage not"
	
	| aClass |
	^ self type = #method and:
			[(aClass := self methodClass) isNil or:
				[(aClass includesSelector: self methodSelector) not]]
]

{ #category : #testing }
ChangeRecord >> isUnchangedMethod [

	^ self isMethodDefinedInImage 
		and: [self string withBlanksCondensed 
				= (self methodClass sourceCodeAt: self methodSelector) asString withBlanksCondensed ]
]

{ #category : #access }
ChangeRecord >> methodClass [
	| methodClass |
	type == #method
		ifFalse: [ ^ nil ].
	(Smalltalk globals includesKey: class asSymbol)
		ifFalse: [ ^ nil ].
	methodClass := Smalltalk globals at: class asSymbol.
	^ meta
		ifTrue: [ methodClass class ]
		ifFalse: [ methodClass ]
]

{ #category : #access }
ChangeRecord >> methodClassName [
	^ meta
		ifTrue: [ class , ' class' ]
		ifFalse: [ class ]
]

{ #category : #access }
ChangeRecord >> methodSelector [
	^ type == #method
		ifTrue: [ (Smalltalk globals at: class ifAbsent: [ Object ]) compiler parseSelector: self string ]
]

{ #category : #access }
ChangeRecord >> position [
	^ position
]

{ #category : #printing }
ChangeRecord >> printOn: aStream [

	aStream 
		nextPutAll: self type printString.
	self stamp ifNotNil: [ aStream nextPutAll: self stamp ]
]

{ #category : #access }
ChangeRecord >> sourceCode [
	
	self type= #preamble ifTrue: [ ^ #preamble printString ].
	self type= #doIt ifTrue: [ ^ #preamble printString ].
	self type= #classComment ifTrue: [ self commentClass ifNotNil: [ :comment | ^ comment comment ]].

	^ self string
]

{ #category : #access }
ChangeRecord >> stamp [
	^ stamp
]

{ #category : #access }
ChangeRecord >> stamp: threePartString [

	stamp := threePartString
]

{ #category : #access }
ChangeRecord >> string [ 
	| copy |
	copy := file readOnlyCopy.
	copy position: position.
	[ ^ (ChunkReadStream on: copy) next ]
		ensure: [ copy close ]
]

{ #category : #access }
ChangeRecord >> timeStamp [
	"Answer a TimeStamp that corresponds to my (text) stamp"

	| tokens |
	tokens := self stamp findTokens: Character separators.
	^ tokens size > 2
		ifTrue: [ 
			[ 
			| time date |
			date := Date fromString: (tokens at: tokens size - 1).
			time := Time fromString: tokens last.
			DateAndTime date: date time: time ]
				on: Error
				do: [ :ex | ex return: DateAndTime new ] ]
		ifFalse: [ DateAndTime new ]
]

{ #category : #access }
ChangeRecord >> type [
	^ type
]
