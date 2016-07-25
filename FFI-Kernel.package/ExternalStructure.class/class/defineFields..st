field definition
defineFields: fields
	"Define all the fields in the receiver"
	self compileFields: fields withAccessors: true.