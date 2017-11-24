"
I am ZnByteStringBecameWideString, a resumable Error signalled to indicate that some byteString was changed to a wideString.

Used by ZnUTF8Encoder>>#readInto:startingAt:count:fromStream: to avoid a #becomeForward: when a ByteString automagically changes into a WideString.

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnByteStringBecameWideString,
	#superclass : #Error,
	#instVars : [
		'byteString',
		'wideString'
	],
	#category : #'Zinc-Character-Encoding-Core'
}

{ #category : #convenience }
ZnByteStringBecameWideString class >> convert: byteString [
	"Convert byteString to a wideString, signalling a resumable error"
	
	| wideString |
	wideString := WideString from: byteString.
	self new
		byteString: byteString;
		wideString: wideString;
		signal.
	^ wideString
]

{ #category : #convenience }
ZnByteStringBecameWideString >> becomeForward [
	"Switch the identities of byteString and wideString using #becomeForward:"

	byteString becomeForward: wideString 
]

{ #category : #accessing }
ZnByteStringBecameWideString >> byteString [
	^ byteString
]

{ #category : #accessing }
ZnByteStringBecameWideString >> byteString: anObject [
	byteString := anObject
]

{ #category : #testing }
ZnByteStringBecameWideString >> isResumable [
	^ true
]

{ #category : #accessing }
ZnByteStringBecameWideString >> wideString [
	^ wideString
]

{ #category : #accessing }
ZnByteStringBecameWideString >> wideString: anObject [
	wideString := anObject
]
