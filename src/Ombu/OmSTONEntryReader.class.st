"
I implement how to read entries from a stream using Ston serializer.
"
Class {
	#name : #OmSTONEntryReader,
	#superclass : #OmEntryReader,
	#instVars : [
		'reader'
	],
	#category : #'Ombu-Persistence'
}

{ #category : #reading }
OmSTONEntryReader >> entryPositionsDo: aBlock [

	[ self
		nextEntryPositionIfFound: [:position |
			aBlock value: position. true ] 
		ifNone: [ false ]
	] whileTrue
]

{ #category : #reading }
OmSTONEntryReader >> entryPositionsReverseDo: aBlock [

	| found token |
	token := 'OmEntry {'.
	stream setToEnd.

	[	[ 	found := stream backUpTo: token. 
			stream skip: token size negated. ] 
				doWhileTrue: [ found and: [ stream peek = $\ ] ].

		found ifFalse: [ ^self ].

		stream skip: token size negated.
		aBlock value: stream position.
			
	] doWhileTrue: true

]

{ #category : #reading }
OmSTONEntryReader >> entryPositionsUpTo: endPosition [

	| positions |
	positions := OrderedCollection new.
	
	self entryPositionsDo: [ :position |
		position > endPosition ifTrue: [ ^positions ].
		positions add: position ].
	
	^ positions
]

{ #category : #reading }
OmSTONEntryReader >> nextEntry [

	^ OmCurrentGlobalName
		value: store globalName
		during: [
			self stonReader
				reset;
				next ]
]

{ #category : #reading }
OmSTONEntryReader >> nextEntryPositionIfFound: foundBlock ifNone: noneBlock [

	| token |
	token := 'OmEntry {'.

	[ [ stream upToAll: token ]
		on: ZnInvalidUTF8
		do: [ :exception | 
			"Workaround: positioned block in middle of multibyte utf8 char 
			   => skip a byte then try again"
			stream next.
			exception retry ].
	stream peek = $\ ] whileTrue.	"here we handle case when log contains this method itself"
	^ stream atEnd
		ifTrue: noneBlock
		ifFalse: [ foundBlock value: stream position - token size ]
]

{ #category : #accessing }
OmSTONEntryReader >> stonReader [

	^ reader ifNil: [ 
		reader := STON reader
			allowComplexMapKeys: true;
			yourself ]
]

{ #category : #accessing }
OmSTONEntryReader >> stream: aReadStream [

	super stream: aReadStream.
	self stonReader on: stream.

]
