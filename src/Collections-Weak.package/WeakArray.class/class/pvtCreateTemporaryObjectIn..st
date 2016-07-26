private
pvtCreateTemporaryObjectIn: tempObject
	"We have to create the temporary object in a separate stack frame"
	tempObject at: 1 put: Object new