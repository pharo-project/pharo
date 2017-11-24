"
I am ZnUtils holding various utilities related to HTTP handling.

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnUtils,
	#superclass : #Object,
	#category : #'Zinc-HTTP-Support'
}

{ #category : #streaming }
ZnUtils class >> bufferedWriteStreamOn: stream [
	^ (ZnBufferedWriteStream on: stream)
		sizeBuffer: self streamingBufferSize;
		yourself
]

{ #category : #conversions }
ZnUtils class >> capitalizeString: string [
	| capitalExpected |
	capitalExpected := true.
	^ String new: string size streamContents: [ :stream |
		string do: [ :each |
			each isAlphaNumeric
				ifTrue: [
					capitalExpected 
						ifTrue: [ 
							stream nextPut: each asUppercase.
							capitalExpected := false. ]
						ifFalse: [ 
							stream nextPut: each asLowercase ] ] 
		 		ifFalse: [ 
					stream nextPut: each.
					capitalExpected := true ] ] ]
]

{ #category : #accessing }
ZnUtils class >> currentProcessID [
	"Return a integer of at most 6 digits that can be used as a description for the current active process.
	This is a reduced hash that is not necessarily unique (but uniqueness is very probably)"
	
	^ Processor activeProcess hash \\ 999999
]

{ #category : #conversions }
ZnUtils class >> decodeBase64: string [
	| bytes |
	bytes := ZnBase64Encoder new decode: string.
	^ ZnNullEncoder new decodeBytes: bytes
]

{ #category : #conversions }
ZnUtils class >> encodeBase64: string [
	| bytes |
	bytes := ZnNullEncoder new encodeString: string.
	^ ZnBase64Encoder new encode: bytes
]

{ #category : #conversions }
ZnUtils class >> httpDate [
	^ self httpDate: DateAndTime now
]

{ #category : #conversions }
ZnUtils class >> httpDate: object [
	"Answers a string formatting object as time stamp for HTTP1.1 (RFC 1123). 
	Eg. Sun, 06 Nov 1994 08:49:37 GMT"

	| utc |
	utc := object asDateAndTime asUTC.
	^ String streamContents: [ :stream |
		stream nextPutAll: utc dayOfWeekAbbreviation; nextPut: $,; space.
		stream nextPutAll: (utc dayOfMonth printStringLength: 2 padded: true).
		stream space; nextPutAll: utc monthAbbreviation; space.
		utc year printOn: stream.
		stream space.
		utc printHMSOn: stream.
		stream nextPutAll: ' GMT' ]
]

{ #category : #testing }
ZnUtils class >> isCapitalizedString: string [
	| capitalExpected |
	capitalExpected := true.
	string do: [ :each |
		each isAlphaNumeric
			ifTrue: [
				((capitalExpected and: [ each isUppercase ])
					or: [ capitalExpected not and: [ each isLowercase ] ])
					ifTrue: [ capitalExpected := false ]
					ifFalse: [ ^ false ] ] 
		 	ifFalse: [ capitalExpected := true ] ].
	^ true




]

{ #category : #streaming }
ZnUtils class >> nextPutAll: collection on: stream [
	| totalWritten toWrite total |
	total := collection size.
	"Do not enter the loop for small sizes"
	total < self streamingBufferSize
		ifTrue: [ ^ stream nextPutAll: collection ].
	totalWritten := 0.
	[ totalWritten < total ] whileTrue: [ 
		toWrite := self streamingBufferSize min: (total - totalWritten).
		stream next: toWrite putAll: collection startingAt: totalWritten + 1.
		totalWritten := totalWritten + toWrite.
		self signalProgress: totalWritten total: total ]
]

{ #category : #conversions }
ZnUtils class >> parseHttpDate: string [
	"self parseHttpDate: 'Tue, 13 Sep 2011 08:04:49 GMT'."
	"self parseHttpDate: 'Tue, 13-Sep-2011 08:04:51 GMT'."
	"self parseHttpDate: 'Tue Jan 01 00:00:01 2036 GMT'."
	
	| tokens day month year hour minute second months map yearToken |
	tokens := (string findTokens: #( $ $- $: $, )) allButFirst.
	tokens last = 'GMT' ifTrue: [ tokens := tokens allButLast ].
	months := #(jan feb mar apr may jun jul aug sep oct nov dec).
	(Integer readFrom: tokens first ifFail: [ 0 ]) isZero
		ifTrue: [ map := #(2 1 6 3 4 5) ]
		ifFalse: [ map := #(1 2 3 4 5 6) ].
	day := Integer readFrom: (tokens at: map first) ifFail: [ 0 ].
	month := months indexOf: (((tokens at: map second) asLowercase) copyFrom: 1 to: 3) ifAbsent: [ 0 ].
	yearToken := tokens at: map third.
	year := yearToken isAllDigits
		ifTrue: [
			year := yearToken asNumber.
			yearToken size = 2
				ifTrue: [
					year < 50
						ifTrue: [ year + 2000 ]
						ifFalse: [ year + 1900 ] ]
				ifFalse: [ year ] ]
		ifFalse: [ 0 ].
	hour := Integer readFrom: (tokens at: map fourth) ifFail: [ 0 ].
	minute := Integer readFrom: (tokens at: map fifth) ifFail: [ 0 ].
	second := Integer readFrom: (tokens at: map sixth) ifFail: [ 0 ].
	^ DateAndTime year: year month: month day: day hour: hour minute: minute second: second offset: Duration zero
]

{ #category : #streaming }
ZnUtils class >> readUpToEnd: inputStream limit: limit [
	| species bufferSize buffer totalRead outputStream |
	bufferSize := limit ifNil: [ self streamingBufferSize ] ifNotNil: [ self streamingBufferSize min: limit ].
	species := inputStream isBinary ifTrue: [ ByteArray ] ifFalse: [ String ].
	buffer := species new: bufferSize.
	totalRead := 0.
	outputStream := nil.
	[ inputStream atEnd ] whileFalse: [ | readCount |
		readCount := inputStream readInto: buffer startingAt: 1 count: bufferSize.
		totalRead := totalRead + readCount.
		(limit notNil and: [ totalRead > limit ])
			ifTrue: [ ZnEntityTooLarge signal ].
		outputStream ifNil: [ 
			inputStream atEnd
				ifTrue: [ ^ buffer copyFrom: 1 to: readCount ] 
				ifFalse: [ outputStream := (species new: bufferSize) writeStream ] ].
		outputStream next: readCount putAll: buffer startingAt: 1.
		self signalProgress: totalRead total: limit ].
	^ outputStream ifNil: [ species new ] ifNotNil: [ outputStream contents ]
]

{ #category : #streaming }
ZnUtils class >> signalProgress: amount total: total [
	ZnSignalProgress enabled
		ifTrue: [ 
			total 
				ifNil: [ 
					HTTPProgress new 
						signal: ('Transferred {1} bytes ...' format: { amount humanReadableSIByteSize }) ]
				ifNotNil: [ 
					HTTPProgress new 
						total: total; 
						amount: amount;
						signal: 'Transferring...' ] ]
]

{ #category : #streaming }
ZnUtils class >> streamFrom: inputStream to: outputStream [
	| buffer totalRead |
	buffer := (inputStream isBinary ifTrue: [ ByteArray ] ifFalse: [ String ]) new: self streamingBufferSize.
	totalRead := 0.
	[ inputStream atEnd ]
		whileFalse: [ | readCount |
			readCount := inputStream readInto: buffer startingAt: 1 count: buffer size.
			readCount > 0
				ifTrue: [
					totalRead := totalRead + readCount.
					outputStream next: readCount putAll: buffer startingAt: 1 ].
			inputStream atEnd
				ifFalse: [
					self signalProgress: totalRead total: nil. 
					outputStream flush ] ]
]

{ #category : #streaming }
ZnUtils class >> streamFrom: inputStream to: outputStream size: totalSize [
	| bufferSize buffer leftToRead |
	bufferSize := self streamingBufferSize min: totalSize.
	buffer := (inputStream isBinary ifTrue: [ ByteArray ] ifFalse: [ String ]) new: bufferSize.
	leftToRead := totalSize.
	[ leftToRead > 0 ]
		whileTrue: [ | readCount |
			readCount := bufferSize min: leftToRead.
			inputStream next: readCount into: buffer.
			leftToRead := leftToRead - readCount.
			outputStream next: readCount putAll: buffer startingAt: 1.
			leftToRead > 0 
				ifTrue: [
					self signalProgress: (totalSize - leftToRead) total: totalSize.
					outputStream flush ] ]
]

{ #category : #streaming }
ZnUtils class >> streamingBufferSize [
	"Return the byte count used as buffer size when streaming"
	
	^ 16384
]

{ #category : #conversions }
ZnUtils class >> trimString: string [
	"Until String>>#trimBoth is available everywhere"
	
	| left right |
	left := 1.
	right := string size.
	[ left <= right and: [ (string at: left) isSeparator ] ]
		whileTrue: [ left := left + 1 ].
	[ left <= right and: [ (string at: right) isSeparator ] ]
		whileTrue: [ right := right - 1 ].
	^ string copyFrom: left to: right
]
