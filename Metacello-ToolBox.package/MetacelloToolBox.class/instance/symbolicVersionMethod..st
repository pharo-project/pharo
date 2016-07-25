api-configuration
symbolicVersionMethod: versionSymbol
	(self hasMethodForSymbolicVersion: versionSymbol)
		ifTrue: [ self modifySymbolicVersionMethodFor: versionSymbol ]
		ifFalse: [ 
			self
				createSymbolicVersionMethod: versionSymbol asString , ':'
				inCategory: 'symbolic versions'
				forVersion: versionSymbol ].
