accessing
sourceCode

	self isActive 
		ifTrue: [ ^ self compiledMethod ifNil:[ sourceCode ] ifNotNil: [ self compiledMethod sourceCode ]].
	self isHistorical
		ifTrue: [ ^ self sourceCodeAtPointer ifNil:[ self compiledMethod ifNil:[ sourceCode ] ifNotNil:[ :cm| cm sourceCode ] ] ].	
	^ sourceCode