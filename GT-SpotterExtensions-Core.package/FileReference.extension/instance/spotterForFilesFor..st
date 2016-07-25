*GT-SpotterExtensions-Core
spotterForFilesFor: aStep
	<spotterOrder: 20>
	^ self isDirectory ifTrue: [
		aStep listProcessor
			allCandidates: [ self files ];
			title: 'Files';
			candidatesLimit: Float infinity;
			itemName: #basename;
			itemIcon: [ GLMUIThemeExtraIcons glamorousBrowse ];
			filter: GTFilterSubstring ]