initialize-release
initializeForms
	"Initialize the receiver's image forms."

	|inactiveForm|
	super initializeForms.
	inactiveForm := self newWindowInactiveControlForm.
	self forms
		at: #windowCloseOver put: self newWindowCloseOverForm;
		at: #windowMinimizeOver put: self newWindowMinimizeOverForm;
		at: #windowMaximizeOver put: self newWindowMaximizeOverForm;
		at: #windowClosePassive put: inactiveForm;
		at: #windowMinimizePassive put: inactiveForm;
		at: #windowMaximizePassive put: inactiveForm