Class {
	#name : #ZnUtilsTests,
	#superclass : #TestCase,
	#category : #Zinc-Tests
}

{ #category : #testing }
ZnUtilsTests >> testBase64 [
	| short long encoded legalCharactes |
	legalCharactes := (String withAll: ($A to: $Z), ($a to: $z), ($0 to: $9)), '='.
	short := String withAll: ((1 to: 16) collect: [ :each | 'abc' atRandom ]).
	encoded := ZnUtils encodeBase64: short.
	self assert: (ZnUtils decodeBase64: encoded) = short.
	self assert: (encoded allSatisfy: [ :each | legalCharactes includes: each ]).
	"We don't want line breaks!"
	long := String withAll: ((1 to: 64) collect: [ :each | 'abc' atRandom ]).
	encoded := ZnUtils encodeBase64: long.
	self assert: (ZnUtils decodeBase64: encoded) = long.
	self assert: (encoded allSatisfy: [ :each | legalCharactes includes: each ])

]

{ #category : #testing }
ZnUtilsTests >> testCapitalizeString [
	self assert: (ZnUtils capitalizeString: 'content-type') = 'Content-Type'.
	self assert: (ZnUtils capitalizeString: 'CONTENT-type') = 'Content-Type'.
	self assert: (ZnUtils capitalizeString: 'content-TYPE') = 'Content-Type'.
	self assert: (ZnUtils capitalizeString: 'WWW-Authenticate') = 'Www-Authenticate'.
	self assert: (ZnUtils capitalizeString: 'Content-MD5') = 'Content-Md5'.
]

{ #category : #testing }
ZnUtilsTests >> testHttpDate [
	| timestamp string |
	timestamp := (DateAndTime year: 2010 month: 9 day: 1 hour: 10 minute: 10 second: 10 offset: 0 seconds).
	string := ZnUtils httpDate: timestamp.
	self assert: (string = 'Wed, 01 Sep 2010 10:10:10 GMT')
	
]

{ #category : #testing }
ZnUtilsTests >> testIsCapitalizedString [
	self assert: (ZnUtils isCapitalizedString: 'Content-Type').
	self assert: (ZnUtils isCapitalizedString: 'CONTENT-type') not.
	self assert: (ZnUtils isCapitalizedString: 'content-TYPE') not.
	self assert: (ZnUtils isCapitalizedString: 'WWW-Authenticate') not.
	self assert: (ZnUtils isCapitalizedString: 'Connection')
]

{ #category : #testing }
ZnUtilsTests >> testNextPutAll [
	| data result |
	data := String new: 64 * 1024 withAll: $A.
	result := String streamContents: [ :out | ZnUtils nextPutAll: data on: out ].
	self assert: result equals: data
]

{ #category : #testing }
ZnUtilsTests >> testParseHttpDate [
	"RFC 822, updated by RFC 1123"
	
	| timeStamp |
	timeStamp := ZnUtils parseHttpDate: 'Tue, 13 Sep 2011 08:04:49 GMT'.
	self assert: timeStamp dayOfMonth = 13.
	self assert: timeStamp monthIndex = 9.
	self assert: timeStamp year = 2011.
	self assert: timeStamp hour = 8.
	self assert: timeStamp minute = 4.
	self assert: timeStamp second = 49.
	self assert: timeStamp offset = Duration zero.
]

{ #category : #testing }
ZnUtilsTests >> testParseHttpDateAlternative1 [
	"ANSI C's asctime() format"
	
	| timeStamp |
	timeStamp := ZnUtils parseHttpDate: 'Tue Jan 01 00:00:01 2036'.
	self assert: timeStamp dayOfMonth = 1.
	self assert: timeStamp monthIndex = 1.
	self assert: timeStamp year = 2036.
	self assert: timeStamp hour = 0.
	self assert: timeStamp minute = 0.
	self assert: timeStamp second = 1.
	self assert: timeStamp offset = Duration zero
]

{ #category : #testing }
ZnUtilsTests >> testParseHttpDateDashes [
	"RFC 850, obsoleted by RFC 1036"
	
	| timeStamp |
	timeStamp := ZnUtils parseHttpDate: 'Tuesday, 13-Sep-11 08:04:49 GMT'.
	self assert: timeStamp dayOfMonth = 13.
	self assert: timeStamp monthIndex = 9.
	self assert: timeStamp year = 2011.
	self assert: timeStamp hour = 8.
	self assert: timeStamp minute = 4.
	self assert: timeStamp second = 49.
	self assert: timeStamp offset = Duration zero
]

{ #category : #testing }
ZnUtilsTests >> testParseHttpDateDashesAlternative1 [
	"RFC 850, obsoleted by RFC 1036"
	
	| timeStamp |
	timeStamp := ZnUtils parseHttpDate: 'Tue, 13-Sep-2011 08:04:49 GMT'.
	self assert: timeStamp dayOfMonth = 13.
	self assert: timeStamp monthIndex = 9.
	self assert: timeStamp year = 2011.
	self assert: timeStamp hour = 8.
	self assert: timeStamp minute = 4.
	self assert: timeStamp second = 49.
	self assert: timeStamp offset = Duration zero
]

{ #category : #testing }
ZnUtilsTests >> testReadUpToEndBinary [
	| data |
	data := ByteArray streamContents: [ :stream | 
		1 to: 10000 do: [ :each | 
			stream nextPut: #(1 2 3) atRandom ] ].
	self assert: (ZnUtils readUpToEnd: data readStream limit: nil) = data.
	self assert: (ZnUtils readUpToEnd: data readStream limit: 10000) = data.
	self should: [ ZnUtils readUpToEnd: data readStream limit: 9999 ] raise: ZnEntityTooLarge 

]

{ #category : #testing }
ZnUtilsTests >> testReadUpToEndNonBinary [
	| data |
	data := String streamContents: [ :stream | 
		1 to: 10000 do: [ :each | 
			stream nextPut: 'abc' atRandom ] ].
	self assert: (ZnUtils readUpToEnd: data readStream limit: nil) = data.
	self assert: (ZnUtils readUpToEnd: data readStream limit: 10000) = data.
	self should: [ ZnUtils readUpToEnd: data readStream limit: 9999 ] raise: ZnEntityTooLarge 

]

{ #category : #testing }
ZnUtilsTests >> testStreamingBinary [
	| data in out |
	data := ByteArray streamContents: [ :stream | 
		1 to: 10000 do: [ :each |
			stream nextPut: #(1 2 3) atRandom ] ].
	in := data readStream.
	out := WriteStream on: (ByteArray new: data size).
	ZnUtils streamFrom: in to: out size: data size.
	self assert: in atEnd.
	self assert: out contents = data 
]

{ #category : #testing }
ZnUtilsTests >> testStreamingBinaryWithoutSize [
	| data in out |
	data := ByteArray streamContents: [ :stream | 
		1 to: 10000 do: [ :each |
			stream nextPut: #(1 2 3) atRandom ] ].
	in := data readStream.
	out := WriteStream on: (ByteArray new: data size).
	ZnUtils streamFrom: in to: out.
	self assert: in atEnd.
	self assert: out contents equals: data 
]

{ #category : #testing }
ZnUtilsTests >> testStreamingNonBinary [
	| data in out |
	data := String streamContents: [ :stream | 
		1 to: 10000 do: [ :each | 
			stream nextPut: 'abc' atRandom ] ].
	in := data readStream.
	out := WriteStream on: (String new: data size).
	ZnUtils streamFrom: in to: out size: data size.
	self assert: in atEnd.
	self assert: out contents = data 
]

{ #category : #testing }
ZnUtilsTests >> testStreamingNonBinaryWithoutSize [
	| data in out |
	data := String streamContents: [ :stream | 
		1 to: 10000 do: [ :each | 
			stream nextPut: 'abc' atRandom ] ].
	in := data readStream.
	out := WriteStream on: (String new: data size).
	ZnUtils streamFrom: in to: out.
	self assert: in atEnd.
	self assert: out contents equals: data 
]
