adding/removing
addPackage: aRGPackage
	"adds a child package"

	aRGPackage isPackage ifFalse:[ ^self ].
	self addElement: aRGPackage in: self packages