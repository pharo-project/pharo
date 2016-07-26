initialization
deleteIfPopUp: evt
	"Recurse up for nested pop ups"
	owner ifNotNil:[owner deleteIfPopUp: evt].