"
I am ZnResourceMetaUtils.

I contain some class side utility methods.

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnResourceMetaUtils,
	#superclass : #Object,
	#category : #'Zinc-Resource-Meta-Core'
}

{ #category : #converting }
ZnResourceMetaUtils class >> decodePercent: string [
	^ ZnPercentEncoder new
		decodePlusAsSpace: false;
		decode: string
]

{ #category : #converting }
ZnResourceMetaUtils class >> decodePercentForQuery: string [
	^ ZnPercentEncoder new
		decodePlusAsSpace: true;
		decode: string
]

{ #category : #converting }
ZnResourceMetaUtils class >> encodePercent: string safeSet: safeSet encoder: encoder [
	^ ZnPercentEncoder new
		characterEncoder: encoder;
		safeSet: (self perform: safeSet);
		encode: string
]

{ #category : #converting }
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
	"There is a tension between encoding as much as possible and leaving as much characters unencoded
	as allowed by the specifications. Since there is no single best answer, this became an option."

	^ ZnCurrentOptions at: #queryKeyValueSafeSet
]

{ #category : #accessing }
ZnResourceMetaUtils class >> querySafeSet [
	"See RFC 3986 section 3.4. The safe set for the query part consists of
	the basic safe set #rfc3986SafeSet plus the 'sub-delimiters' !$&''()*+,;=
	plus : and @ plus / and ?"

	^ 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_.~!$&''()*+,;=:@/?'
]

{ #category : #accessing }
ZnResourceMetaUtils class >> rfc3986SafeSet [
	"Return the unreserved characters according to RFC 3986 section 2.3.
	This is the most narrow safe set to be used in a better safe than sorry approach."

	^ ZnPercentEncoder rfc3986UnreservedCharacters
]

{ #category : #accessing }
ZnResourceMetaUtils class >> urlPathSafeSet [
	"See RFC 3986 section 3.3. The safe set for the path consists of
	the basic safe set #rfc3986SafeSet plus the 'sub-delimiters' !$&''()*+,;= plus : and @"

	^ 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_.~!$&''()*+,;=:@'
]

{ #category : #converting }
ZnResourceMetaUtils class >> writeQueryFields: dictionary on: stream [
	self writeQueryFields: dictionary withEncoder: ZnCharacterEncoder utf8 on: stream
]

{ #category : #converting }
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
