*Iceberg-UI
icon
	^ self isExtension
		ifTrue: [ self targetClass icon ]
		ifFalse: [ Smalltalk ui icons iconNamed: #protocolExtension ] 
