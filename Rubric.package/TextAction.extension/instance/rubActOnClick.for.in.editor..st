*Rubric-Editing-Core
rubActOnClick: anEvent for: anObject in: paragraph editor: editor
	"sent when a user clicks on a piece of text to which I am applied in an editor"

	"may be self is included in the event or an Object. "
	^ actOnClickBlock valueWithEnoughArguments: (Array with: self with: anEvent with: anObject with: paragraph with: editor)