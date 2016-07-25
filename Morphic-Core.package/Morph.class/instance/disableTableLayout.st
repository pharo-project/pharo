layout-properties
disableTableLayout
	"Layout specific. Disable laying out the receiver in table layout"
	| props |
	props := self layoutProperties.
	^props ifNil:[false] ifNotNil:[props disableTableLayout].