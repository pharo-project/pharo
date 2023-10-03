"
I implement how to read entries from a stream using Ston serializer.
"
Class {
	#name : 'OmSTONEntryReader',
	#superclass : 'OmEntryReader',
	#instVars : [
		'reader'
	],
	#category : 'Ombu-Persistence',
	#package : 'Ombu',
	#tag : 'Persistence'
}

{ #category : 'reading' }
OmSTONEntryReader >> entryPositionsDo: aBlock [

	[ self nextEntryPositionIfFound: [ :position | aBlock value: position ] ifNone: [ ^ self ] ] repeat
]

{ #category : 'reading' }
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

{ #category : 'reading' }
OmSTONEntryReader >> entryPositionsUpTo: endPosition [

	| positions |
	positions := OrderedCollection new.

	self entryPositionsDo: [ :position |
		position > endPosition ifTrue: [ ^positions ].
		positions add: position ].

	^ positions
]

{ #category : 'reading' }
OmSTONEntryReader >> nextEntry [

	^ OmCurrentGlobalName
		value: store globalName
		during: [
			self stonReader
				reset;
				next ]
]

{ #category : 'reading' }
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
		ifTrue: [ noneBlock value ]
		ifFalse: [ foundBlock value: stream position - token size ]
]

{ #category : 'accessing' }
OmSTONEntryReader >> positionAtNextEntry [
	"Consume readStream up to the start of the next OmEntry.
	Return true if an entry was found."

	| entryPrefix |
	"Only the first entry may be not preceded by a cr character"
	(stream position = 0 and: [ stream match: 'OmEntry' ]) ifTrue: [
		stream position: 0.
		^ true ].

	"Otherwise, the cr is mandatory."
	entryPrefix := '
OmEntry {'.
	^ (stream match: entryPrefix)
		  ifTrue: [
			  stream position: stream position - entryPrefix size.
			  true ]
		  ifFalse: [ false ]
]

{ #category : 'accessing' }
OmSTONEntryReader >> positionAtNextEntryStartingAt: aPosition [
	"Consume reamStream up to the start of the next OmEntry. Note that aPosition may indicate the middle of multibyte UTF-8 character, that's tolerated by skipping forward the bytes.
	Return true if an entry was found."

	stream positionForward: aPosition.

	^ self positionAtNextEntry
]

{ #category : 'accessing' }
OmSTONEntryReader >> stonReader [

	^ reader ifNil: [
		reader := STON reader
			allowComplexMapKeys: true;
			yourself ]
]

{ #category : 'accessing' }
OmSTONEntryReader >> stream: aReadStream [

	super stream: aReadStream.
	self stonReader on: stream
]
