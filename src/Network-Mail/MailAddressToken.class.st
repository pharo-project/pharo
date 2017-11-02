"
a single token from an RFC822 mail address.  Used internally in MailAddressParser
"
Class {
	#name : #MailAddressToken,
	#superclass : #Object,
	#instVars : [
		'type',
		'text'
	],
	#category : 'Network-Mail'
}

{ #category : #'instance creation' }
MailAddressToken class >> type: type  text: text [
	^self new type: type text: text
]

{ #category : #printing }
MailAddressToken >> printOn: aStream [
	aStream nextPut: $[.
	aStream nextPutAll: self type asString.
	aStream nextPut: $|.
	aStream nextPutAll: self text.
	aStream nextPut: $].
]

{ #category : #access }
MailAddressToken >> text [
	^text
]

{ #category : #access }
MailAddressToken >> type [
	^type
]

{ #category : #private }
MailAddressToken >> type: type0  text: text0 [
	type := type0.
	text := text0.
]
