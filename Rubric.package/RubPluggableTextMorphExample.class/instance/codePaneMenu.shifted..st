menu
codePaneMenu: aMenu shifted: shifted
	"Note that unless we override perform:orSendTo:, 
	PluggableTextController will respond to all menu items in a 
	text pane"
	"You should not use this way of getting menu. Because when you create a text area, you have a menu"

	| donorMenu |
	donorMenu := shifted
		ifTrue: [ SmalltalkEditor shiftedYellowButtonMenu ]
		ifFalse: [ SmalltalkEditor yellowButtonMenu ].
	^ aMenu addAllFrom: donorMenu