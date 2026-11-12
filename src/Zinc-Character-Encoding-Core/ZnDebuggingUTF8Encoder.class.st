Class {
	#name : 'ZnDebuggingUTF8Encoder',
	#superclass : 'ZnLossyUTF8Encoder',
	#instVars : [
		'writeErrorBlock'
	],
	#category : 'Zinc-Character-Encoding-Core',
	#package : 'Zinc-Character-Encoding-Core'
}

{ #category : 'accessing' }
ZnDebuggingUTF8Encoder class >> handlesEncoding: string [
	"Return true when my instances handle the encoding described by string"

	^ (self canonicalEncodingIdentifier: string) = 'utf8debug'
]

{ #category : 'accessing' }
ZnDebuggingUTF8Encoder class >> knownEncodingIdentifiers [
	^ #( 'utf8debug' )
]

{ #category : 'accessing' }
ZnDebuggingUTF8Encoder >> defaultWriteErrorBlock [
	^ [ :codePoint | 
		'<Cannot utf-8 encode code point {1}>' format: { codePoint } ]
]

{ #category : 'error handling' }
ZnDebuggingUTF8Encoder >> errorOutsideRange: codePoint to: stream [
	| errorMessage |
	errorMessage := self writeErrorBlock value: codePoint.
	errorMessage isByteString
		ifFalse: [ errorMessage := self defaultWriteErrorBlock value: codePoint ].
	stream nextPutAll: errorMessage asByteArray
]

{ #category : 'accessing' }
ZnDebuggingUTF8Encoder >> identifier [
	^ 'utf8debug'
]

{ #category : 'accessing' }
ZnDebuggingUTF8Encoder >> writeErrorBlock [
	^ writeErrorBlock ifNil: [ writeErrorBlock := self defaultWriteErrorBlock ]
]

{ #category : 'accessing' }
ZnDebuggingUTF8Encoder >> writeErrorBlock: aOneArgumentBlock [
	writeErrorBlock := aOneArgumentBlock
]
