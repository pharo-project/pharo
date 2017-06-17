*Iceberg-UI
icon
	^ self classDefinition 
		ifNotNil: #icon
		ifNil: [	self isExtension
			ifTrue: [ Smalltalk ui icons iconNamed: #protocolExtension ] 
			ifFalse: [ self targetClass icon ]]
