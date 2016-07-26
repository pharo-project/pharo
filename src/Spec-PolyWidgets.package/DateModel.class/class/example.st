example
example
	"self example inspect"

	| widget |	
	widget := self new.
	(widget openDialogWithSpec: #spec) modalRelativeTo: self currentWorld.
	^ widget value