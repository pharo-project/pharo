argv building
collectArgumentPointersInto: aPointer
	"Allocates each string argument with a null terminated char and add them into aPointer"
	
	"#allButLast is because the last is 0."
	self argVArguments allButLast withIndexDo: [ :string :index |
		| pointer | "0 terminated"
		pointer := ExternalAddress allocate: string size + 1.
		self registerPointer: pointer.
		LibC memCopy: string to: pointer size: string size.
		pointer nbUInt8AtOffset: string size put: 0. "string terminating null char"
		aPointer nbUInt32AtOffset: (index - 1) * self systemAccessor sizeOfPointer put: pointer value ].

	aPointer nbUInt32AtOffset: (self argVArguments size - 1) * self systemAccessor sizeOfPointer put: 0