*GT-SpotterExtensions-Core
spotterPreviewZipItemsIn: aComposite
	<spotterPreview: 10>

	| zip |
	self isZipFile ifFalse: [ ^ self ].
	zip:= (FileSystem zip: self) open workingDirectory.
	aComposite list
		title: [ self gtDisplayString ];
		display: #yourself;
		format: [ :folder | 
			GTSpotterItemBrick new 
				text: folder basename; 
				icon: (folder isDirectory 
								ifTrue: [Smalltalk ui theme icons glamorousFolder]
								ifFalse: [Smalltalk ui theme icons glamorousBrowse] ); 
				disable; 
				yourself ];
		styled: [ :brick | brick ];
		entity: (zip directories, zip files)