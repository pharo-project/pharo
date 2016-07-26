accessing
menuOn: aBuilder

	<RubSmalltalkCodeMenu>
	
	aBuilder
		item: 'Suggestions...';
		selector: #value;
		target: [SugsMenuBuilder showMenuFor: aBuilder model];
		keyText: 't ';
		order: 0;
		icon: (Smalltalk ui icons iconNamed: #smallAuthoringTools);
		with: [ SugsMenuBuilder buildContextMenuOn: aBuilder]