accessing
renameChangesForClass: oldClassName to: newClassName
	"We're in the middle of performing a rename operation. If we stored the class name, we need to change the class name to the new name to perform the compiles."

	self subclassResponsibility