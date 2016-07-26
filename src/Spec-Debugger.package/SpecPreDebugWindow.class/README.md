I am the window that shows up when the debugger is launched.  
I show you the first lines of the stack trace that caused my apparition (or a text editor with a message), and a set of buttons created by loading all debugging actions annotated with the pragmas 'self preDebuggingActionsPragmas' (e.g. resume the execution, abandon the execution).

To see me, evaluate:
2/0.

Some details about my implementation.

- I hold a reference to the debugger than created my;
- changing the debuger retriggers the construction of this window;
- before doing this users should set the attribute 'message' as my structure depens on it;
- changing the debugger should be done only after this window was open. E.g.:

SpecPreDebugWindow new 
		setTitle: aTitle;
		message: aMessage;
		openWithSpec;
		debugger: aDebugger.
		
- an example of manually opening this window:

[ | context process  debugger |

context := [ 20 factorial ] asContext.

process := Process 
	forContext: context
	priority: Processor userInterruptPriority.

debugger := SpecDebugger new
	process: process 
	controller:  nil
	context: context.
	
SpecPreDebugWindow new 
		setTitle: 'A simulated error';
		message: nil;
		openWithSpec;
		debugger: debugger
 ] fork.