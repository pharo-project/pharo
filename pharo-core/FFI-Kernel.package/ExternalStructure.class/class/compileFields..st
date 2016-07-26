field definition
compileFields: fieldSpec
	"Compile the field definition of the receiver.
	Return the newly compiled spec."
	^self compileFields: fieldSpec withAccessors: false.