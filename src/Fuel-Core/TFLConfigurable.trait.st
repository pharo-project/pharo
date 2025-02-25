Trait {
	#name : 'TFLConfigurable',
	#category : 'Fuel-Core-Configuration',
	#package : 'Fuel-Core',
	#tag : 'Configuration'
}

{ #category : 'accessing' }
TFLConfigurable classSide >> configuration [
	^ FLCurrentConfiguration value
]

{ #category : 'accessing' }
TFLConfigurable classSide >> context [
	^ FLCurrentContext value
]

{ #category : 'accessing' }
TFLConfigurable >> configuration [
	^ self class configuration
]

{ #category : 'accessing' }
TFLConfigurable >> context [
	^ self class context
]

{ #category : 'hooks' }
TFLConfigurable >> fuelAccept: aGeneralMapper [
	^ aGeneralMapper
		visitSubstitution: self
		by: nil
]
