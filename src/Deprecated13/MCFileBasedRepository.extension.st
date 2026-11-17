Extension { #name : 'MCFileBasedRepository' }

{ #category : '*Deprecated13' }
MCFileBasedRepository >> allFileNamesOrCache [

	self
		deprecated: 'Use #allFileNames instead.'
		in: #Pharo13
		transformWith: '`@rcv allFileNamesOrCache' -> '`@rcv allFileNames'.
	^ self allFileNames
]
