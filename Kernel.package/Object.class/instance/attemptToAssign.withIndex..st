write barrier
attemptToAssign: value withIndex: index 
	"Called by the VM when assigning an instance variable of an immutable object.
	Upon return, executing will resume *after* the inst var assignment. If the inst var mutation has to be 
	performed, do it manually here in the call back with instVarAt:put: .
	This method has to return *no* value. I do it by hacking the process  until we provide a better solution."
	
	| process |
	
	"self do something here if you want. Typically, raise a NoModification error. For example:
	NoModification new
		object: self;
		index: index;
		value: value;
		selector: #instVarAt:put:
		signal
	The selector #instVarAt:put: is passed in the NoModification error in this method, whereas on primitive failure (at:put: failure for example)
	the primitive selector is passed. This way while error handling block it is possible to ask the object to perform the mutation using the
	selector, the index and the value. For example:
	[ ... ] on: NoModificationError do: 
		[ :ex | 
			self logMutation: ex. 
			ex object beWritableObject.
			ex object perform: ex selector withArgs: { ex index . ex value }.
			ex object beReadOnlyObject ]."
	
	process := Processor activeProcess.
	[ process suspendedContext: process suspendedContext sender ] forkAt: Processor activePriority + 1.
	Processor yield.
	"CAN'T REACH"