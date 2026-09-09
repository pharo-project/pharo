Extension { #name : 'ReCreateAccessorsForVariableTransformation' }

{ #category : '*Deprecated14' }
ReCreateAccessorsForVariableTransformation >> getterMethod [

	self deprecated: 'Use getterMethodName' transformWith: '`@arg getterMethod' -> '`@arg getterMethodName'.
	^ self getterMethodName
]

{ #category : '*Deprecated14' }
ReCreateAccessorsForVariableTransformation >> setterMethod [
	self deprecated: 'Use getterMethodName' transformWith: '`@arg setterMethod' -> '`@arg setterMethodName'.

	^ self setterMethodName
]
