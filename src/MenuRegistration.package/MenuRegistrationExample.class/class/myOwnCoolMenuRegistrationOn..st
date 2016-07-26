menu declaring
myOwnCoolMenuRegistrationOn: aBuilder 
	<worldMenuExample>
	(aBuilder item: #MyOwnCoolMenu)
		parent: #Tools; "place me as a child of the #Tools node (declared by #toolsOn:)"
		label: 'The coolest tool here';
		with: [
			(aBuilder item: #MyOwnCoolTool)
				label: 'My own cool tool also here';
				target: Smalltalk tools workspace;
				selector: #openContents: ;
				arguments: #('yep, my own cool tool can be also opened from a world sub-menu ! :)')]