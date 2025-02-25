Extension { #name : 'MCFileBasedRepository' }

{ #category : '*Deprecated13' }
MCFileBasedRepository >> allFileNamesOrCache [

	self deprecated: 'Use #allFileNames instead.' transformWith: '`@rcv allFileNamesOrCache' -> '`@rcv allFileNames'.
	^ self allFileNames
]
