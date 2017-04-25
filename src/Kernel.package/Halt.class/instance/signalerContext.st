accessing
signalerContext
	"specialized version to find the proper context to open the debugger on.
	This will find the first context whose method is no longer on Halt or Halt class nor is #halt method iteself."
	^ signalContext findContextSuchThat: [ :context |
		(context receiver == self 
		or: [ (context receiver == self class) 
		or: [ context method hasPragmaNamed: #debuggerCompleteToSender ]]) not ]