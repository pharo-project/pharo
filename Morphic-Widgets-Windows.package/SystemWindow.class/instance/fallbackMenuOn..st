menu building
fallbackMenuOn: menu
	"Build the menu that is put up if something is going wrong with the menubuilder"
	
	menu
		addToggle: 'Close' translated
		target: self
		selector: #closeBoxHit
		getStateSelector: nil
		enablementSelector: #allowedToClose.
	menu lastItem icon: self theme windowCloseForm.