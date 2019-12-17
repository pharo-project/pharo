Class {
	#name : #ZnHeadersTest,
	#superclass : #TestCase,
	#category : #'Zinc-Tests'
}

{ #category : #testing }
ZnHeadersTest >> testMergeValued [
	| headers |
	headers := ZnHeaders new.
	headers at: 'Set-Cookie' put: 'foo=1'.
	headers at: 'Set-Cookie' put: 'bar=1' ifPresentMerge: [ :old :new | old, ';', new ].
	self assert: (headers at: 'Set-Cookie') equals: 'foo=1;bar=1'
	
	
]

{ #category : #testing }
ZnHeadersTest >> testMultiValued [
	| headers |
	headers := ZnHeaders new.
	headers at: 'Set-Cookie' put: 'foo=1'.
	headers at: 'Set-Cookie' add: 'bar=1'.
	self assert: (headers at: 'Set-Cookie') equals: #('foo=1' 'bar=1')
	
	
]

{ #category : #testing }
ZnHeadersTest >> testMultiValuedStreaming [
	| headers keysAndValues |
	headers := ZnHeaders new.
	headers at: 'Set-Cookie' put: 'foo=1'.
	headers at: 'Set-Cookie' add: 'bar=1'.
	keysAndValues := OrderedCollection streamContents: [ : stream |
		headers headersDo: [ :key :value |
			stream nextPut: key; nextPut: value ] ].
	self assert: keysAndValues equals: #('Set-Cookie' 'foo=1' 'Set-Cookie' 'bar=1') asOrderedCollection 
	
	
]

{ #category : #testing }
ZnHeadersTest >> testNormalization [
	| headers |
	headers := ZnHeaders new.
	headers at: 'content-TYPE' put: 'text/plain'.
	self assert: (headers includesKey: 'content-type').
	self assert: (headers includesKey: 'content-TYPE').
	self assert: (headers at: 'Content-Type') equals: 'text/plain'.
	headers at: 'CONTENT-type' put: 'text/html'.
	self assert: (headers at: 'Content-Type') equals: 'text/html'.

	
]

{ #category : #testing }
ZnHeadersTest >> testReading [
	| string headers |
	string := 'Content-Type: text/plain', String crlf, 'Content-Length: 128', String crlf.
	headers := ZnHeaders readFrom: string asByteArray readStream.
	self assert: (headers includesKey: 'Content-Type').
	self assert: (headers includesKey: 'Content-Length').
	self assert: (headers at: 'Content-Type') equals: 'text/plain'.
	self assert: (headers at: 'Content-Length') equals: '128'.
]

{ #category : #testing }
ZnHeadersTest >> testReadingMultiline [
	| string headers |
	string := 'Content-Type: text/plain', String crlf, 
		'Long: foo', String crlf, 
		' bar', String crlf, 
		String tab, 'baz', String crlf, 
		'Content-Length: 128', String crlf.
	headers := ZnHeaders readFrom: string asByteArray readStream.
	self assert: (headers includesKey: 'Content-Type').
	self assert: (headers includesKey: 'Content-Length').
	self assert: (headers at: 'Content-Type') equals: 'text/plain'.
	self assert: (headers at: 'Content-Length') equals: '128'.
	self assert: (headers includesKey: #Long).
	self assert: (headers at: #long) equals: 'foo bar	baz'.
]

{ #category : #testing }
ZnHeadersTest >> testReadingMultilineMultiValue [
	| string headers |
	string := 'Content-Type: text/plain', String crlf, 
		'Long: foo', String crlf, 
		' bar', String crlf, 
		String tab, 'baz', String crlf, 
		'Content-Length: 128', String crlf,
		'Long: more', String crlf.
	headers := ZnHeaders readFrom: string asByteArray readStream.
	self assert: (headers at: #long) equals: #( 'foo bar	baz' 'more').
]

{ #category : #testing }
ZnHeadersTest >> testReadingMultilineNonBinary [
	| string headers |
	string := 'Foo: 1', String crlf, 
		' 2', String crlf, 
		String tab, '3', String crlf, 
		'Bar: EOF', String crlf.
	headers := ZnHeaders readFrom: string readStream.
	self assert: (headers at: 'Foo') equals: '1 2', String tab, '3'.
	self assert: (headers at: 'Bar') equals: 'EOF'
]

{ #category : #testing }
ZnHeadersTest >> testWriting [
	| headers string |
	headers := ZnHeaders new
		at: 'Content-Type' put: 'text/plain';
		at: 'Content-Length' put: '128';
		yourself.
	string := String streamContents: [ :stream | headers writeOn: stream ].
	self assert: (string includesSubstring: 'Content-Type: text/plain' , String crlf).
	self assert: (string includesSubstring: 'Content-Length: 128' , String crlf).
	self assert: (string occurrencesOf: Character cr) equals: 2.
	self assert: (string occurrencesOf: Character lf) equals: 2
]
