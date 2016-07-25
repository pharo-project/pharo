user-interface
buildWindowMenu
	"Build and answer the window menu."

	| aMenu |
	aMenu := self theme newMenuIn: self for: self.
	aMenu
		addToggle: 'Close' translated
		target: self
		selector: #closeBoxHit
		getStateSelector: nil
		enablementSelector: #allowedToClose.
	aMenu lastItem icon: self theme windowCloseForm.
	aMenu addLine.
	aMenu add: 'About' selector: #showAbout.
	aMenu lastItem icon: self theme smallHelpIcon.
	^ aMenu