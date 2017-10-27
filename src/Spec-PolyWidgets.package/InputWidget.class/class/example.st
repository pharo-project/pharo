example
example
	"self example inspect"

	| widget |	
	widget := self new.
	widget
		title: 'Name';
		label: 'What''s your name?';
		ghostText: 'John Doe'.
	widget openDialogWithSpec modalRelativeTo: self currentWorld.
	^ widget value