layout-properties
reverseTableCells
	"Layout specific. This property describes if the cells should be treated in reverse order of submorphs."
	| props |
	props := self layoutProperties.
	^props ifNil:[false] ifNotNil:[props reverseTableCells].