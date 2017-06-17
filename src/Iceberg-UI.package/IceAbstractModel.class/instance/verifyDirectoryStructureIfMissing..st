private
verifyDirectoryStructureIfMissing: aBlock
	self repository isMissing 
		ifTrue: [ ^ aBlock value: (self makeError: 'Local repository missing') ].
	self repository isCodeMissing
		ifTrue: [ ^ aBlock value: (self makeError: 'Code directory missing') ].
