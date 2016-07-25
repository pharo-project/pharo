*GT-SpotterExtensions-Core
spotterForStashedPlaygroundPagesFor: aStep
	<spotterOrder: 100>
	aStep listProcessor
			allCandidates: [ 
				| stashFiles |
				stashFiles := GTPlayBook stashDirectory files 
					select: [ :each | each extension = GTPlayPage fileExtension ].
				stashFiles collect: [ :file |
					(GTPlayPage fromFileReference: file) 
						title: (file basename copyUpTo: $-) ] ];
			title: 'Playground named pages';
			itemName: [ :page | page title, ' - ', page content asString contractTo: 100 ];
			filter: GTFilterSubstring