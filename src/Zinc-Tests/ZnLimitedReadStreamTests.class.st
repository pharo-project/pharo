Class {
	#name : #ZnLimitedReadStreamTests,
	#superclass : #TestCase,
	#category : 'Zinc-Tests'
}

{ #category : #testing }
ZnLimitedReadStreamTests >> testBinary [
	| data limitedReadStream |
	data := #(0 1 2 3 4 5 6 7 8 9) asByteArray.
	limitedReadStream := ZnLimitedReadStream on: data readStream limit: 5.
	self assert: limitedReadStream upToEnd = #(0 1 2 3 4) asByteArray
]

{ #category : #testing }
ZnLimitedReadStreamTests >> testBinaryAll [
	| data limitedReadStream |
	data := #(0 1 2 3 4 5 6 7 8 9) asByteArray.
	limitedReadStream := ZnLimitedReadStream on: data readStream limit: data size.
	self assert: limitedReadStream upToEnd = data
]

{ #category : #testing }
ZnLimitedReadStreamTests >> testMatch [
	| stream |
	stream := ZnLimitedReadStream on: 'ababcabc' readStream limit: 8.
	self assert: (stream match: 'ab').
	self deny: stream atEnd.
	stream := ZnLimitedReadStream on: 'ababcabc' readStream limit: 8.
	self assert: (stream match: 'abc').
	self deny: stream atEnd.
	self assert: (stream match: 'abc').
	self assert: stream atEnd.
	stream := ZnLimitedReadStream on: 'ababcabc' readStream limit: 8.
	self deny: (stream match: 'xx').
	self assert: stream atEnd.

]

{ #category : #testing }
ZnLimitedReadStreamTests >> testNext [
	| limitedReadStream |
	limitedReadStream := ZnLimitedReadStream on: String new readStream limit: 10.
	self assert: limitedReadStream atEnd.
	self assert: limitedReadStream next isNil.
	limitedReadStream := ZnLimitedReadStream on: 'ABC' readStream limit: 10.
	self assert: limitedReadStream atEnd not.
	self assert: limitedReadStream next = $A.
	self assert: limitedReadStream peek = $B.
	limitedReadStream next.
	self assert: limitedReadStream next = $C.
	self assert: limitedReadStream next isNil.
	self assert: limitedReadStream atEnd
	
]

{ #category : #testing }
ZnLimitedReadStreamTests >> testNextCount [
	| data stream |
	data := '0123456789'.
	stream := ZnLimitedReadStream on: data readStream limit: 8.
	self assert: (stream next: 4) = '0123'
]

{ #category : #testing }
ZnLimitedReadStreamTests >> testNextCountInto [
	| data stream buffer |
	data := '0123456789'.
	stream := ZnLimitedReadStream on: data readStream limit: 8.
	buffer := String new: 4.
	self assert: (stream next: 4 into: buffer) = '0123'.
	self assert: buffer = '0123'
]

{ #category : #testing }
ZnLimitedReadStreamTests >> testNextCountIntoShort [
	| data stream buffer |
	data := '0123456789'.
	stream := ZnLimitedReadStream on: data readStream limit: 5.
	buffer := String new: 6.
	self assert: (stream next: 6 into: buffer) = '01234'
	
]

{ #category : #testing }
ZnLimitedReadStreamTests >> testNextCountShort [
	| data stream |
	data := '0123456789'.
	stream := ZnLimitedReadStream on: data readStream limit: 5.
	self assert: (stream next: 6) = '01234'
]

{ #category : #testing }
ZnLimitedReadStreamTests >> testSimple [
	| data limitedReadStream |
	data := '0123456789'.
	limitedReadStream := ZnLimitedReadStream on: data readStream limit: 5.
	self assert: limitedReadStream upToEnd = '01234'
]

{ #category : #testing }
ZnLimitedReadStreamTests >> testSimpleEof [
	| data limitedReadStream |
	data := '0123456789'.
	limitedReadStream := ZnLimitedReadStream on: data readStream limit: 15.
	self assert: limitedReadStream upToEnd = '0123456789'.
	self assert: limitedReadStream atEnd.
	self assert: limitedReadStream next isNil
]
