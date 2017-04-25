building
addField: registerClass size: fieldSize alignment: fieldAlignment
	self alignTo: fieldSize.
	fields add: (FFIExternalStructureFlatLayoutField new
			registerClass: registerClass;
			size: fieldSize;
			alignment: fieldAlignment;
			offset: size
			yourself
		).
	size := size + fieldSize.