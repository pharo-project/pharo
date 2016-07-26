initialize-release
initialize: syntaxTreeRoot ignoreCase: aBoolean
	"Compile thyself for the regex with the specified syntax tree.
	See comment and `building' protocol in this class and 
	#dispatchTo: methods in syntax tree components for details 
	on double-dispatch building. 
	The argument is supposedly a RxsRegex."

	ignoreCase := aBoolean.
	self buildFrom: syntaxTreeRoot.
	startOptimizer := RxMatchOptimizer new initialize: syntaxTreeRoot ignoreCase: aBoolean