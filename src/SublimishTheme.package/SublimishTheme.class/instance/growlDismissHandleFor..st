growl - specific
growlDismissHandleFor: aGrowlMorph
	| form image |
	form := self windowCloseForm.
	image := ImageMorph new.
	image image: form.
	image color: Color yellow.
	^ image