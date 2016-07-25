private
getEnv: aVariableName
	| valueSize buffer |
	valueSize := self getEnvSize: aVariableName.
	valueSize = 0 
		ifTrue: [ ^ nil ].
	buffer := ByteArray new: valueSize.
	(self getEnv: aVariableName buffer: buffer size: valueSize) = (valueSize - 1)
		ifFalse: [ ^ nil ].
	^ buffer allButLast asString