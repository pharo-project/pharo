menu declaration
menuOn: aBuilder
	"Specify the menu used when writing text. Try it with:
	(PragmaMenuBuilder 
		pragmaKeyword: 'RubLineNumberMenu'
		model: nil) menu popUpInWorld"

	<contextMenu>
	<RubLineNumberMenu>
	(aBuilder item: #'Find...' translated)
		keyText: 'f';
		selector: #find;
		icon: (Smalltalk ui icons iconNamed: #smallFindIcon)
