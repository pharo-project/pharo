private
directoryFromEnvVariableNamed: aString or: aBlock
	| envValue |
	envValue := [ Smalltalk os environment at: aString ifAbsent: [ nil ] ]
		on: Error
		do: [ ^ aBlock value ].
	^ envValue isEmptyOrNil
		ifTrue: [ aBlock value ]
		ifFalse: [ self resolveString: envValue ]