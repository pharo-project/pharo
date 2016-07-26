*GT-SpotterExtensions-Core
spotterForZipDirectoriesFor: aStep
	<spotterOrder: 10>
	| zipFileSystem |
	self isZipFile ifFalse: [ ^ self ].
	aStep listProcessor
			allCandidates: [ 
				zipFileSystem := (FileSystem zip: self) open.
				zipFileSystem workingDirectory directories ];
			title: 'Directories';
			candidatesLimit: Float infinity;
			itemName: #basename;
			itemIcon: [ GLMUIThemeExtraIcons glamorousFolder ];
			filter: GTFilterSubstring