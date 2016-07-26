removing
remove: oldObject ifAbsent: anExceptionBlock 
	"SequencableCollections cannot implement removing."

	self shouldNotImplement