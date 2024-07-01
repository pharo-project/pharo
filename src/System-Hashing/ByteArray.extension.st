Extension { #name : 'ByteArray' }

{ #category : '*System-Hashing-Core' }
ByteArray >> destroy [
	1 to: self size do:
		[ : x |
		self at: x put: 0 ]
]
