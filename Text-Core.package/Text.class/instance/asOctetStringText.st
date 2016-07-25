converting
asOctetStringText

	string class == WideString ifTrue: [
		^ self class string: string asOctetString runs: self runs copy.
	].
	^self.
