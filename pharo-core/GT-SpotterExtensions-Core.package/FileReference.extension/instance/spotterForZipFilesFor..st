*GT-SpotterExtensions-Core
spotterForZipFilesFor: aStep
	<spotterOrder: 10>
	| zipFileSystem |
	self isZipFile ifFalse: [ ^ self ].
	aStep listProcessor
			allCandidates: [ 
				zipFileSystem := (FileSystem zip: self) open.
				zipFileSystem workingDirectory files ];
			title: 'Files';
			candidatesLimit: Float infinity;
			itemName: #basename;
			itemIcon: [ GLMUIThemeExtraIcons glamorousBrowse ];
			filter: GTFilterSubstring
	