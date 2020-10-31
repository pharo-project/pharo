Extension { #name : #FreeTypeFontFamilyMember }

{ #category : #'*Deprecated90' }
FreeTypeFontFamilyMember >> simulated [
	"Answer the value of isSimulatedStyle"
	self
		deprecated: 'Use #isSimulatedStyle instead'
		on: '31 oct 2020'
		in: #Pharo9
		transformWith: '`@receiver simulated' 
						-> '`@receiver isSimulatedStyle'. 

	^self isSimulatedStyle 
]

{ #category : #'*Deprecated90' }
FreeTypeFontFamilyMember >> simulated: anObject [
	"Set the value of simulated"

	self deprecated: 'Please use #beSimulatedStyle from Pharo 9 onwards'.
	
	isSimulatedStyle := anObject
]
