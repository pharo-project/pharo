private - settings
addDebugFilterSessingsOn: aBuilder
	| stackWidgetClass |
	
	stackWidgetClass := Smalltalk tools debugger stackWidgetClass.
	(aBuilder group: #BasicFilters)
				label: 'Basic Filters';
				description: 'default filtering configurations';
				target: Smalltalk tools debugger;
				with: [
					(aBuilder setting: #filterCommonMessageSends)
				label: 'Filter out common message sends';
				target: Smalltalk tools debugger;
				description: 'When true, filter out uninteresting message sends in the Debugger view while debugging';
				with: [ 
					(aBuilder setting: #filterDoItSelectors)
					label: 'Filter out doIt sends';
					target: stackWidgetClass.
					
					(aBuilder setting: #filterNilSelectors)
					label: 'Filter out nil message sends';
					target: stackWidgetClass.
					
					(aBuilder setting: #filterKernelClasses)
					label: 'Filter out kernel classes message sends';
					target: stackWidgetClass.
					
					(aBuilder setting: #filterLinkSelectors)
					label: 'Filter out link and breakpoint message sends';
					target: stackWidgetClass.
					
					 ]
				].