Extension { #name : 'Integer' }

{ #category : '*Deprecated12' }
Integer >> humanReadableSIByteSize [
	"Return the receiver as a string with SI binary (International System of Units) file size, e.g. '50 KB'"

	self
		deprecated: 'use humanReadableSISizeString'
		transformWith:  '`@rec humanReadableSIByteSize' ->  '`@rec humanReadableSISizeString'.

	^ self humanReadableSISizeString
]
