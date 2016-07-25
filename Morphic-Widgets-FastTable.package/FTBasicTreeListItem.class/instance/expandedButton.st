button
expandedButton

	^self defaultButton
		actionSelector: #collapseItem;
		labelGraphic: Smalltalk ui theme treeExpandedForm;
		helpText: 'Collapse Item';
		yourself