"
I check reading and writing the optional fields of GZIP headers (RFC 1952, section 2.3.1) through GZipHeader: file name, comment, modification time, extra field, operating system and text flag.
"
Class {
	#name : 'GZipHeaderTest',
	#superclass : 'TestCase',
	#category : 'Compression-Tests-Streams',
	#package : 'Compression-Tests',
	#tag : 'Streams'
}

{ #category : 'helpers' }
GZipHeaderTest >> gzip: aString header: aGZipHeader [

	^ ByteArray streamContents: [ :stream |
			  (GZipWriteStream on: stream header: aGZipHeader)
				  nextPutAll: aString asByteArray;
				  close ]
]

{ #category : 'helpers' }
GZipHeaderTest >> pharoGzippedByPython [
	"Bytes written by Python 3:
	gzip.GzipFile(filename='notes.txt', mode='wb', fileobj=buffer, mtime=1715941230).write(b'Pharo')"

	^ #[ 31 139 8 8 110 47 71 102 2 255 110 111 116 101 115 46 116 120 116 0 11 200 72 44 202 7 0 144 33 178 137 5 0 0 0 ]
]

{ #category : 'tests' }
GZipHeaderTest >> testDefaultHeader [
	"Without header, nothing optional is written and the operating system is unknown (255)"

	| bytes reader |
	bytes := ByteArray streamContents: [ :stream |
			         (GZipWriteStream on: stream)
				         nextPutAll: 'Hello' asByteArray;
				         close ].
	self assert: (bytes first: 10) equals: #[ 31 139 8 0 0 0 0 0 0 255 ].
	reader := GZipReadStream on: bytes.
	self assert: reader header fileName isNil.
	self assert: reader header modificationTime isNil.
	self assert: reader header operatingSystem equals: 255.
	self assert: reader upToEnd asString equals: 'Hello'
]

{ #category : 'tests' }
GZipHeaderTest >> testFileNameIsLatin1 [

	| bytes |
	bytes := self gzip: 'Hello' header: (GZipHeader new
			          fileName: 'café.txt';
			          yourself).
	"The name follows the 10 bytes of the fixed header"
	self assert: (bytes copyFrom: 11 to: 19) equals: #[ 99 97 102 16rE9 46 116 120 116 0 ].
	self assert: (GZipReadStream on: bytes) header fileName equals: 'café.txt'
]

{ #category : 'tests' }
GZipHeaderTest >> testFileNameOutsideLatin1IsRejected [

	self
		should: [
				self gzip: 'Hello' header: (GZipHeader new
						 fileName: (String with: (Character value: 16r3A9));
						 yourself) ]
		raise: Error.
	self
		should: [
				self gzip: 'Hello' header: (GZipHeader new
						 comment: (String with: (Character value: 0));
						 yourself) ]
		raise: Error
]

{ #category : 'tests' }
GZipHeaderTest >> testHeaderOfFirstMemberIsAnswered [

	| bytes reader |
	bytes := (self gzip: 'Hello ' header: (GZipHeader new
			           fileName: 'first.txt';
			           yourself)) , (self gzip: 'World' header: (GZipHeader new
			           fileName: 'second.txt';
			           yourself)).
	reader := GZipReadStream on: bytes.
	self assert: reader header fileName equals: 'first.txt'.
	self assert: reader upToEnd asString equals: 'Hello World'
]

{ #category : 'tests' }
GZipHeaderTest >> testReadsFileNameWrittenByGzip [

	self assert: (GZipReadStream on: GZipReadStreamTest gzFileBytes) header fileName equals: 'test.txt'
]

{ #category : 'tests' }
GZipHeaderTest >> testReadsHeaderWrittenByPython [

	| reader |
	reader := GZipReadStream on: self pharoGzippedByPython.
	self assert: reader header fileName equals: 'notes.txt'.
	self assert: reader header modificationTime equals: (DateAndTime
			 year: 2024
			 month: 5
			 day: 17
			 hour: 10
			 minute: 20
			 second: 30
			 offset: Duration zero).
	self assert: reader header operatingSystem equals: 255.
	self assert: reader header comment isNil.
	self assert: reader header extraField isNil.
	self deny: reader header isText.
	self assert: reader upToEnd asString equals: 'Pharo'
]

{ #category : 'tests' }
GZipHeaderTest >> testWritesAndReadsAllFields [

	| time header reader |
	time := DateAndTime
		        year: 2024
		        month: 5
		        day: 17
		        hour: 10
		        minute: 20
		        second: 30
		        offset: Duration zero.
	header := GZipHeader new
		          fileName: 'notes.txt';
		          comment: 'a comment';
		          modificationTime: time;
		          extraField: #[ 65 66 2 0 1 2 ];
		          operatingSystem: 3;
		          isText: true;
		          yourself.
	reader := GZipReadStream on: (self gzip: 'Hello' header: header).
	self assert: reader header fileName equals: 'notes.txt'.
	self assert: reader header comment equals: 'a comment'.
	self assert: reader header modificationTime equals: time.
	self assert: reader header extraField equals: #[ 65 66 2 0 1 2 ].
	self assert: reader header operatingSystem equals: 3.
	self assert: reader header isText.
	self assert: reader upToEnd asString equals: 'Hello'
]
