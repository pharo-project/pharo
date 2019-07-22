"
Specialized version of inspector showing the hex and string representation of the byte array in some variations.

"
Class {
	#name : #EyeByteArrayInspector,
	#superclass : #EyeCollectionInspector,
	#category : #'Spec2-Inspector-Base'
}

{ #category : #list }
EyeByteArrayInspector >> addSpecialFields: elements [
	elements add: (DynamicEyeElement 
		host: self object 
		label: 'hex bytes' 
		description: [ self objectAsHexBytes ]
		value: [ self object ]).
	elements add: (DynamicEyeElement 
		host: self object 
		label: 'hex string' 
		value: [ self objectAsHexString ]).
	elements add: (DynamicEyeElement 
		host: self object 
		label: 'latin-1 string' 
		value: [ self objectAsLatin1String ]).
	elements add: (DynamicEyeElement 
		host: self object 
		label: 'utf-8 string' 
		value: [ 
			[ self objectAsUTF8String ] 
				on: ZnInvalidUTF8 
				do: [ :exception | exception asString ] ]).
]

{ #category : #converting }
EyeByteArrayInspector >> objectAsHexBytes [
	^ String
		streamContents: [ :s | 
			s nextPutAll: '['.
			self object 
				do: [ :each | s << (each printPaddedWith: $0 to: 2 base: 16) ] 
				separatedBy: [ s nextPut: $  ].
			s nextPut: $] ]
		limitedTo: 5000
]

{ #category : #converting }
EyeByteArrayInspector >> objectAsHexString [
	^ String
		streamContents: [ :s | 
			self object do: [ :each | s << (each printPaddedWith: $0 to: 2 base: 16) ] ]
		limitedTo: 5000
]

{ #category : #converting }
EyeByteArrayInspector >> objectAsLatin1String [
	| width str |
	str := String new:
		(width := self object size min: 5000).
	str
		replaceFrom: 1
		to: str size
		with: (self object copyFrom: 1 to: width).
	^ str
]

{ #category : #converting }
EyeByteArrayInspector >> objectAsUTF8String [
	^ String
		streamContents: [ :out | 
			| in encoder |
			encoder := ZnUTF8Encoder new.
			in := self object readStream.
			[ in atEnd ] whileFalse: [ 
				out nextPut: (encoder nextFromStream: in) ] ]
		limitedTo: 5000
]
