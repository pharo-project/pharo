Extension { #name : 'String' }

{ #category : '*Deprecated12' }
String class >> crlfcrlf [

	self
		deprecated: 'Will be removed without replacement in next iteration'
		on: '21 September 2023'
		in:
		'Pharo-12.0.0+build.779.sha.e06ae6a0b17ea62c2031af00a73fd2471d563666 (64 Bit)'.
	^ self crlf , self crlf
]
