Class {
	#name : #ZnLimitedReadStreamTest,
	#superclass : #TestCase,
	#category : #'Zinc-Tests'
}

{ #category : #testing }
ZnLimitedReadStreamTest >> testBinary [
	| data limitedReadStream |
	data := #(0 1 2 3 4 5 6 7 8 9) asByteArray.
	limitedReadStream := ZnLimitedReadStream on: data readStream limit: 5.
	self assert: limitedReadStream upToEnd equals: #(0 1 2 3 4) asByteArray
]

{ #category : #testing }
ZnLimitedReadStreamTest >> testBinaryAll [
	| data limitedReadStream |
	data := #(0 1 2 3 4 5 6 7 8 9) asByteArray.
	limitedReadStream := ZnLimitedReadStream on: data readStream limit: data size.
	self assert: limitedReadStream upToEnd equals: data
]

{ #category : #testing }
ZnLimitedReadStreamTest >> testMatch [
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
ZnLimitedReadStreamTest >> testNext [
	| limitedReadStream |
	limitedReadStream := ZnLimitedReadStream on: String new readStream limit: 10.
	self assert: limitedReadStream atEnd.
	self assert: limitedReadStream next isNil.
	limitedReadStream := ZnLimitedReadStream on: 'ABC' readStream limit: 10.
	self assert: limitedReadStream atEnd not.
	self assert: limitedReadStream next equals: $A.
	self assert: limitedReadStream peek equals: $B.
	limitedReadStream next.
	self assert: limitedReadStream next equals: $C.
	self assert: limitedReadStream next isNil.
	self assert: limitedReadStream atEnd
]

{ #category : #testing }
ZnLimitedReadStreamTest >> testNextCount [
	| data stream |
	data := '0123456789'.
	stream := ZnLimitedReadStream on: data readStream limit: 8.
	self assert: (stream next: 4) equals: '0123'
]

{ #category : #testing }
ZnLimitedReadStreamTest >> testNextCountInto [
	| data stream buffer |
	data := '0123456789'.
	stream := ZnLimitedReadStream on: data readStream limit: 8.
	buffer := String new: 4.
	self assert: (stream next: 4 into: buffer) equals: '0123'.
	self assert: buffer equals: '0123'
]

{ #category : #testing }
ZnLimitedReadStreamTest >> testNextCountIntoShort [
	| data stream buffer |
	data := '0123456789'.
	stream := ZnLimitedReadStream on: data readStream limit: 5.
	buffer := String new: 6.
	self assert: (stream next: 6 into: buffer) equals: '01234'
]

{ #category : #testing }
ZnLimitedReadStreamTest >> testNextCountShort [
	| data stream |
	data := '0123456789'.
	stream := ZnLimitedReadStream on: data readStream limit: 5.
	self assert: (stream next: 6) equals: '01234'
]

{ #category : #testing }
ZnLimitedReadStreamTest >> testSimple [
	| data limitedReadStream |
	data := '0123456789'.
	limitedReadStream := ZnLimitedReadStream on: data readStream limit: 5.
	self assert: limitedReadStream upToEnd equals: '01234'
]

{ #category : #testing }
ZnLimitedReadStreamTest >> testSimpleEof [
	| data limitedReadStream |
	data := '0123456789'.
	limitedReadStream := ZnLimitedReadStream on: data readStream limit: 15.
	self assert: limitedReadStream upToEnd equals: '0123456789'.
	self assert: limitedReadStream atEnd.
	self assert: limitedReadStream next isNil
]
