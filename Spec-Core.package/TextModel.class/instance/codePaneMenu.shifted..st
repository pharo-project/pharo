api
codePaneMenu: aMenu shifted: shifted 
	"Note that unless we override perform:orSendTo:, 
	PluggableTextController will respond to all menu items in a 
	text pane"
	
	^ menuHolder value cull: aMenu cull: shifted