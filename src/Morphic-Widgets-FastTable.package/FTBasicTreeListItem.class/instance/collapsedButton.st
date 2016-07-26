button
collapsedButton

	^self defaultButton
		actionSelector: #expandItem;
		labelGraphic: Smalltalk ui theme treeUnexpandedForm;
		helpText: 'Expand Item';
		yourself