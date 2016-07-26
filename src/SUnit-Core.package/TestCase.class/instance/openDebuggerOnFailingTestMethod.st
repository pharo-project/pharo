running
openDebuggerOnFailingTestMethod
	"SUnit has halted one step in front of the failing test method. Step over the 'self halt' and 
	 send into 'self perform: testSelector' to see the failure from the beginning"

	self
		halt;
		performTest