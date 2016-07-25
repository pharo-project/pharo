default actions
unhandledErrorDefaultAction: anException

	"Provide a default handling for unhandled error. 
	We should never reach this code, because it should be overridden by subclasses. 
	(but we can't put 'self subclassResponsibility' , because it will cause infinite loop,
	in case if current ui manager having no override of this method).
	This method is only for documentation purposes."
		
	Smalltalk snapshot: false andQuit: true.