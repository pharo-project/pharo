*GT-SpotterExtensions-Core
spotterCompositionFor: aStep
	<spotterOrder: 25>
	aStep listProcessor
			title: 'Composing traits';
			allCandidates: [ self traitComposition allTraits ];
			itemIcon: #systemIcon;
			filter: GTFilterSubstring