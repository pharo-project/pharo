"
I am ZnResourceMetaUtils.

I contain some class side utility methods.

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnResourceMetaUtils,
	#superclass : #Object,
	#category : #Zinc-Resource-Meta-Core
}

{ #category : #conversions }
ZnResourceMetaUtils class >> decodePercent: string [
	^ ZnPercentEncoder new 
		decodePlusAsSpace: false; 
		decode: string
]

{ #category : #conversions }
ZnResourceMetaUtils class >> decodePercentForQuery: string [
	^ ZnPercentEncoder new 
		decodePlusAsSpace: true; 
		decode: string
]

{ #category : #conversions }
ZnResourceMetaUtils class >> encodePercent: string safeSet: safeSet encoder: encoder [
	^ ZnPercentEncoder new
		characterEncoder: encoder;
		safeSet: (self perform: safeSet);
		encode: string
]

{ #category : #conversions }
ZnResourceMetaUtils class >> parseQueryFrom: stream [
	| string index fields |
	fields := ZnMultiValueDictionary new.
	[ stream atEnd ] whileFalse: [
		string := stream upTo: $&.
		index := string indexOf: $=.
		index > 0
			ifFalse: [ fields at: (self decodePercentForQuery: string) put: nil ]
			ifTrue: [
				fields
					at: (self decodePercentForQuery: (string copyFrom: 1 to: index - 1))
					add: (self decodePercentForQuery: (string copyFrom: index + 1 to: string size)) ] ].
	^ fields
]

{ #category : #accessing }
ZnResourceMetaUtils class >> queryKeyValueSafeSet [
	"When a query is interpreted as a list of key=value&.. pairs,
	it is better to encode = and & and leave them out of the safe set.
	Furthermore, since + is interpreted as space in the query part,
	it is unsafe as well"
	
	^ 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_.~!$''()*,;:@/?'
]

{ #category : #accessing }
ZnResourceMetaUtils class >> querySafeSet [
	^ 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_.~!$&''()*+,;=:@/?'
]

{ #category : #accessing }
ZnResourceMetaUtils class >> rfc3986SafeSet [
	^ ZnPercentEncoder rfc3986UnreservedCharacters 
]

{ #category : #accessing }
ZnResourceMetaUtils class >> urlPathSafeSet [
	^ 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_.~!$&''()*+,;=:@'
]

{ #category : #conversions }
ZnResourceMetaUtils class >> writeQueryFields: dictionary on: stream [
	self writeQueryFields: dictionary withEncoder: ZnCharacterEncoder utf8 on: stream 
]

{ #category : #conversions }
ZnResourceMetaUtils class >> writeQueryFields: dictionary withEncoder: encoder on: stream [
	| first |
	first := true.
	dictionary keysAndValuesDo: [ :key :value | 
		first ifFalse: [ stream nextPut: $& ].
		first := false.
		stream nextPutAll: (self encodePercent: key safeSet: #queryKeyValueSafeSet encoder: encoder).
		value ifNotNil: [ 
			stream
				nextPut: $=;
				nextPutAll: (self encodePercent: value safeSet: #queryKeyValueSafeSet encoder: encoder) ] ]
]
