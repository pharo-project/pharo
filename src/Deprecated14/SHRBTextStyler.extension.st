Extension { #name : 'SHRBTextStyler' }

{ #category : '*Deprecated14' }
SHRBTextStyler >> isForWorkspace [

	self
		deprecated: 'use isScripting'
		in: #Pharo14
		transformWith: '`@receiver isForWorkspace' -> '`@receiver isScripting'.
	^ self isScripting
]

{ #category : '*Deprecated14' }
SHRBTextStyler >> isForWorkspace: aBoolean [
		self
			deprecated: 'use isScripting:'
			in: #Pharo14
			transformWith: '`@receiver isForWorkspace: `@argument' -> '`@receiver isScripting: `@argument'.
	^ self isScripting: aBoolean
]
