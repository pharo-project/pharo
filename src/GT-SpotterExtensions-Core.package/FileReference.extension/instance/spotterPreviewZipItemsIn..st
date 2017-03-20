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
			|iconName |			
			iconName := folder isDirectory 
				ifTrue: [ #glamorousFolder ]
				ifFalse: [ #glamorousBrowse ].
				
			GTSpotterItemBrick new 
				text: folder basename; 
				icon: (self iconNamed: iconName); 
				disable; 
				yourself ];
		styled: [ :brick | brick ];
		entity: (zip directories, zip files)