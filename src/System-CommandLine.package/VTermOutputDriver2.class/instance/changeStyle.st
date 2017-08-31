styles
changeStyle
	"Force the changes of style on the outStream"

	self style = installedStyle
		ifFalse: [ self style setInDriver: self ]