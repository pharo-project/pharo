control
deleteIfPopUp: evt 
	evt
		ifNotNil: [evt hand releaseMouseFocus: self]