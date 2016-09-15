icon
categoryIconFor: aString
	(aString beginsWith: '*')
		ifTrue: [ ^ self iconNamed: #protocolExtensionIcon ].
	(aString beginsWith: 'private')
		ifTrue: [ ^ self iconNamed: #protocolPrivateIcon ].
	(aString = 'initialization'
		or: [ aString = 'initialize-release' or: [ aString = 'finalization' ] ])
		ifTrue: [ ^ self iconNamed: #protocolProtectedIcon ].
	^ self iconNamed: #protocolNoneIcon