*GT-SpotterExtensions-Core
spotterForMetacelloConfigurationsFor: aStep
	<spotterOrder: 86>
	aStep listProcessor
			title: 'Metacello Configurations';
			allCandidates: [ 
				Smalltalk allClasses select: [ :each | 
					(each respondsTo: #isMetacelloConfig) 
						and: [ each isMetacelloConfig
							and: [ each name beginsWith: 'ConfigurationOf' ] ] ]
					thenCollect: #project ];
			itemName: [ :each | each configuration class name ];
			itemIcon: [ Smalltalk ui icons iconNamed: #config ];
			filter: GTFilterSubstring;
			wantsToDisplayOnEmptyQuery: false