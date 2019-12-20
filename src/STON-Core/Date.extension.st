Extension { #name : #Date }

{ #category : #'*ston-core' }
Date class >> fromSton: stonReader [
	"Read a ISO YYYY-MM-DD format.
	Since my current implementation is time zone offset sensitive, the offset has to be taken into account.
	A missing offset results in the local timezone offset to be used"
	
	| readStream date |
	readStream := stonReader parseListSingleton readStream.
	date := self readFrom: readStream.
	readStream atEnd
		ifFalse: [ | offset |
			offset := DateAndTime readTimezoneOffsetFrom: readStream.
			offset = date offset 
				ifFalse: [ date start: (date start translateTo: offset) ] ].
	^ date
]

{ #category : #'*ston-core' }
Date >> stonContainSubObjects [ 
	^ false
]

{ #category : #'*ston-core' }
Date >> stonOn: stonWriter [
	"Use an ISO style YYYY-MM-DD representation.
	Since my current implementation is time zone offset sensitive, the offset has to be included."

	| representation |
	representation := self offset isZero
		ifTrue: [ 
			String new: 11 streamContents: [ :out | 
				self printOn: out format: #(3 2 1 $- 1 1 2).
				out nextPut: $Z ] ]
		ifFalse: [ 
			String new: 32 streamContents: [ :out | 
				self printOn: out format: #(3 2 1 $- 1 1 2).
				out nextPut: (self offset positive ifTrue: [ $+ ] ifFalse: [ $- ]).
				self offset hours abs printOn: out base: 10 length: 2 padded: true.
				out nextPut: $:.
				self offset minutes abs printOn: out base: 10 length: 2 padded: true.
				self offset seconds = 0 
					ifFalse:[ 
						out nextPut: $:; print: self offset seconds abs truncated ] ] ].
	stonWriter writeObject: self listSingleton: representation
]
