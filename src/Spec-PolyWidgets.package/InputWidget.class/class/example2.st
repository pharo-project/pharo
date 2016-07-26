example
example2
	"self example2"
	
	| widget buttonModel buttonWindow |
	buttonModel := ButtonModel new.
	buttonWindow := buttonModel openWithSpec.
	widget := InputWidget new.
	widget
		title: 'Label?';
		label: 'Enter a button label';
		ghostText: 'Ok'.
	widget openDialogWithSpec modalRelativeTo: buttonWindow.
	buttonModel label: widget value