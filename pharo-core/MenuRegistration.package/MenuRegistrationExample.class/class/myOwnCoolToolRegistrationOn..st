menu declaring
myOwnCoolToolRegistrationOn: aBuilder 
	<worldMenuExample> 
	(aBuilder item: #MyOwnCoolTool)
		parent: #Tools;
		label: 'My own cool tool';
		target: Smalltalk tools workspace; 
		selector: #openContents: ;
		arguments: #('yep, my own cool tool can be opened from the world menu ! :)')
