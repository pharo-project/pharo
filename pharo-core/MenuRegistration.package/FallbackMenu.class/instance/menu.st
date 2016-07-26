private
menu
	"Build the menu that is put up if something is going wrong with the menubuilder"

	| menu |
	menu := UIManager default newMenuIn: ActiveWorld for: self.
	menu buildTitle: [ :menuTitle | menuTitle
		onlyCloseAndPinable
	].
	menu add: 'Why you see this menu'
		target: self
		selector: #fallbackMenuExplanations.
	menu add: 'Debug'
		target: self
		selector: #debug.
	menu addLine.
	client fallbackMenuOn: menu.
	^ menu