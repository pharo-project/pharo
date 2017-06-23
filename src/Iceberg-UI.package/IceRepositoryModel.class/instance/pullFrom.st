actions
pullFrom
	| remote |
	self validateCanPull ifFalse: [ ^ self ].
	remote := self pickRemote: 'Pull from'.
	remote ifNil: [ ^ self ].
	self repository pullFrom: remote.