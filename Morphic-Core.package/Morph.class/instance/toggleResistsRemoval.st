accessing
toggleResistsRemoval
	"Toggle the resistsRemoval property"

	self resistsRemoval
		ifTrue:
			[self removeProperty: #resistsRemoval]
		ifFalse:
			[self setProperty: #resistsRemoval toValue: true]