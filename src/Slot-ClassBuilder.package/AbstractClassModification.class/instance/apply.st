public
apply
	^ newClass := target shallowCopy
		superclass: self superclass
		layout: layout