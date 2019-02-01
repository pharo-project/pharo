Extension { #name : #Array }

{ #category : #'*System-Settings-Browser' }
Array >> settingStoreOn: aStream [ 
	"Use the literal form if possible."

	self isLiteral
		ifTrue: 
			[aStream nextPut: $#; nextPut: $(.
			self do: 
				[:element | 
				element settingStoreOn: aStream.
				aStream space].
			aStream nextPut: $)]
		ifFalse: [super settingStoreOn: aStream]
]
