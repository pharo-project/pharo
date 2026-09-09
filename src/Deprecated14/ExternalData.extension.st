Extension { #name : 'ExternalData' }

{ #category : '*Deprecated14' }
ExternalData >> readString [
	"Assume that the receiver represents a C string containing UTF8 characters and convert
	 it to a Smalltalk string."

	self
		deprecated:  'Please use #bytesFromCString instead'
		transformWith:  '`@receiver readString'
						-> '`@receiver bytesFromCString asString'.

	self isNull ifTrue: [ ^ nil ].

	^ self bytesFromCString asString
]

{ #category : '*Deprecated14' }
ExternalData >> readStringUTF8 [
	"Assume that the receiver represents a C string containing UTF8 characters and convert
	 it to a Smalltalk string."

	self
		deprecated:  'Please use #utf8StringFromCString instead'
		transformWith:  '`@receiver readStringUTF8'
						-> '`@receiver utf8StringFromCString'.

	^ self bytesFromCString utf8Decoded
]
