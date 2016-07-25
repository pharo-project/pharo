accessing editor
editor
	"Return my current editor, or install a new one."
	^ editor ifNil: [ editor := self newEditor ]