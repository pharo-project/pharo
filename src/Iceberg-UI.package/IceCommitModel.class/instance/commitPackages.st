actions
commitPackages
	| text |
	(text := self message getText) ifEmpty: [ ^ self ].
	
	self basicCommitWithMessage: text ifCancel: [ ^ self ]