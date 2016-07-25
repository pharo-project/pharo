testing
includesCodeChanges

	self entriesDo: [ :each | each content isCodeChange ifTrue: [ ^true ] ].
	
	^false