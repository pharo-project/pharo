actions
compositeText
	"Answer the composite text based on the selection state
	of the joins."

	|t|
	t := Text new.
	self mappings do: [:j |
		j appendToCompositeText: t].
	^t