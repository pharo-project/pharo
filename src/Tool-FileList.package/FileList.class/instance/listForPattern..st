volume list and pattern
listForPattern: aPattern
	"Make the list be those file names which match the pattern."

	| newList |
	newList := self filesMatching: aPattern.
	newList := newList asArray sort: self sortBlock.
	^ newList 