menu
menuOn: aBuilder
	"Specify the menu used when writing code. Try it with:
	(PragmaMenuBuilder 
		pragmaKeyword: 'RubSmalltalkCommentMenu'
		model: RubEditingArea new) menu popUpInWorld
	"

	<contextMenu>
	<RubSmalltalkCommentMenu>
	RubSmalltalkCodeMode menuOn: aBuilder 