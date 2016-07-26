to clean later
ifEmpty: aBlock ifNotEmpty: aBlock2
	^ presenters
		ifEmpty: aBlock
		ifNotEmpty: aBlock2