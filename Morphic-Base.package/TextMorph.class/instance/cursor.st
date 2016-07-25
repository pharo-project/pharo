accessing
cursor
	"Answer the receiver's logical cursor position"

	| loc |
	loc := self valueOfProperty: #textCursorLocation  ifAbsentPut: [1].
	loc := loc min: text string size.
	^ loc rounded
	