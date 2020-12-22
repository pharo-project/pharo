Extension { #name : #TextStyleAsFontFamilyMember }

{ #category : #'*Deprecated90' }
TextStyleAsFontFamilyMember >> simulated [ 
	self
		deprecated: 'Use #isSimulatedStyle instead'
		on: '31 oct 2020'
		in: #Pharo9
		transformWith: '`@receiver simulated' 
						-> '`@receiver isSimulatedStyle'. 
	^self isSimulatedStyle 
	
]
