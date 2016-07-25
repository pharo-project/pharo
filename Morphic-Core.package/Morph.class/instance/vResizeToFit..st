layout-properties
vResizeToFit: aBoolean
	aBoolean ifTrue:[
		self vResizing: #shrinkWrap.
	] ifFalse:[
		self vResizing: #rigid.
	].