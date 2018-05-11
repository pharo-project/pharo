Extension { #name : #ByteArray }

{ #category : #'*Alien-Core' }
ByteArray >> utf8ToIso [
	| s i v1 v2 v3 v4 |
	s := WriteStream on: (String new: self size).
	i := 1.
	[i <= self size] whileTrue:
		[v1 := self at: i.
		 i := i + 1.
		 v1 < 128
			ifTrue: [ s nextPut: v1 asCharacter ]
			ifFalse:
				[(i > self size) ifTrue:
					[self error: 'illegal end-of-string, expected 2nd byte of UTF-8'].
				 v2 := self at: i.
				 i := i + 1.
				 v1 < 224
					ifTrue:
						[s nextPut: ((v2 bitAnd: 63) bitOr: ((v1 bitAnd: 31) bitShift: 6)) asCharacter]
					ifFalse:
						[(i > self size) ifTrue:
							[self error: 'illegal end-of-string, expected 3rd byte of UTF-8'].
						 v3 := self at: i.
						 i := i + 1.
						 v1 < 240
							ifTrue:
								[s nextPut: ((v3 bitAnd: 63) bitOr: (((v2 bitAnd: 63) bitShift: 6) bitOr: ((v1 bitAnd: 31) bitShift: 12))) asCharacter]
							ifFalse:
								[(i > self size) ifTrue:
							[self error: 'illegal end-of-string, expected 4th byte of UTF-8'].
							 v4 := self at: i.
							 i := i + 1.
							 v1 < 248
									ifTrue: [s nextPut: ((v4 bitAnd: 63) bitOr: (((v3 bitAnd: 63) bitShift: 6) bitOr: (((v2 bitAnd: 63) bitShift: 12) bitOr: ((v1 bitAnd: 15) bitShift: 18)))) asCharacter]
									ifFalse:
										[self error: 'illegal UTF-8 character']]]]].
	^s contents
]
