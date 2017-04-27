private
handleErrorDuring: aBlock
	"TODO: do not catch *all* Errors.
	This error handler exists as a workaround to skip any unexpected error when applying or reverting a code change from GUI.
	For example, when user reverts the protocol addition of a protocol that is already not present, and such action signals an unexpected error. The error in such case should be avoided via testing API."
	
	aBlock
		on: Error
		do: self errorHandlerBlock
