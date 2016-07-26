accessing
codePaneMenu: aMenu shifted: shifted
	"Note that unless we override perform:orSendTo:, 
	PluggableTextController will respond to all menu items in a 
	text pane"

	| donorMenu |
	donorMenu := (PragmaMenuBuilder pragmaKeyword: RubSmalltalkCodeMode menuKeyword model: self)
		menu.
	^ aMenu addAllFrom: donorMenu