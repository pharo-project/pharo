write barrier
attemptToAssign: value withIndex: index 
	"Called by the VM when assigning an instance variable of an immutable object.
	Upon return, executing will resume *after* the inst var assignment. If the inst var mutation has to be 
	performed, do it manually here in the call back with instVarAt:put:.
	This method has to return *no* value by jumping to the context's sender"
	
	NoModificationError new
		object: self;
		index: index;
		value: value;
		selector: #instVarAt:put: ;
		signal.

	thisContext sender jump
	"CAN'T REACH"