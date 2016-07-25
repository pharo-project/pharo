adding/removing
removePackage: aRGPackage
	"removes a child aRGPackage"

	aRGPackage isPackage ifFalse:[ ^self ].
	self removeElement: aRGPackage from: self packages