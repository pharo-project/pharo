*GT-SpotterExtensions-Core
spotterForDirectoriesFor: aStep
	<spotterOrder: 10>
	^ self isDirectory ifTrue: [
		aStep listProcessor
			allCandidates: [ self directories ];
			title: 'Directories';
			candidatesLimit: Float infinity;
			itemName: #basename;
			itemIcon: [ GLMUIThemeExtraIcons glamorousFolder ];
			filter: GTFilterSubstring ]