Extension { #name : 'ReCreateAccessorsForVariableTransformation' }

{ #category : '*Deprecated14' }
ReCreateAccessorsForVariableTransformation >> getterMethod [

	self
		deprecated: 'Use getterMethodName'
		in: #Pharo14
		transformWith: '`@arg getterMethod' -> '`@arg getterMethodName'.
	^ self getterMethodName
]

{ #category : '*Deprecated14' }
ReCreateAccessorsForVariableTransformation >> setterMethod [
	self
		deprecated: 'Use getterMethodName'
		in: #Pharo14
		transformWith: '`@arg setterMethod' -> '`@arg setterMethodName'.

	^ self setterMethodName
]
