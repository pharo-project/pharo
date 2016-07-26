A DebugAction is the entry point for creating debugging actions.

A new debugging action is created by subclassing it and implementing, by default, the method executeAction (This can be configured by using the method actionSelector).  An id must also be provided uniquely identifying the action among all the others.

The initialize method should only set default values or initialize attributes that are independent of the debugger or session. The others must be initilized in the method forDebugger:.

A debugging action has the following lifecycle: 
	- the action is created using #new and #initialized is called
	- #appliesToDebugger:, and for contextual actions #appliesToContext: are called
	- if the actions applies to the current situation #forDebugger: is used to set the debugger
	- #execute is called when the user triggers the action.
	- when the debugger updates its action this process is repeated.