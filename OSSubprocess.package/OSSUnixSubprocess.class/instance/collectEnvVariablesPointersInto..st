env building
collectEnvVariablesPointersInto: aPointer
	"Creates the pair of KEY=VALUE string, allocating the memory and adding a null terminated char to each of the env.
	Each processed pair of KEY=VALUE is added to aPointer. "
	envVariables keys withIndexDo: [ :aKey :index |
		| string pointer | 
		string := aKey trimmed , '=', (envVariables at: aKey) trimmed. 
		"The +1 is because string must be NULL terminated"
		pointer := ExternalAddress allocate: string size + 1.
		self registerPointer: pointer.
		LibC memCopy: string to: pointer size: string size.
		pointer nbUInt8AtOffset: string size put: 0. "string terminating null char"
		aPointer nbUInt32AtOffset: (index - 1) * self systemAccessor sizeOfPointer put: pointer value
	].
	"The array of pointers also needs to finish with a NULL"
	aPointer nbUInt32AtOffset: envVariables keys size * self systemAccessor sizeOfPointer put: 0
