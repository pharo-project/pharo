character shapes
characterForm: char pixelValueAt: pt put: val 
	| f |
	f := self characterFormAt: char.
	f 
		pixelAt: pt
		put: val.
	self 
		characterFormAt: char
		put: val