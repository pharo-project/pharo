accessing
allSiblingItems
	"Answer the receiver's submorphs followed by the (wrapping) owner's
	submorph items. Nasty."

	|menus str index|
	str := (Array new: 40) writeStream.
	menus := self owner submorphs select: [:m | m isKindOf: self class].
	menus := (menus copyFrom: (index := menus indexOf: self) to: menus size), (menus copyFrom: 1 to: index - 1).
	menus do: [:menu |
		str nextPutAll: menu items].
	^str contents