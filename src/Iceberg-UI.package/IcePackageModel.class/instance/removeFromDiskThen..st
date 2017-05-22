actions
removeFromDiskThen: aBlock
	(UIManager default confirm: ('You are going to remove package {1} from disk. Are you sure?' format: { self entity name }))
		ifFalse: [ ^ self ].
	self entity removeFromDisk: true.
	aBlock value