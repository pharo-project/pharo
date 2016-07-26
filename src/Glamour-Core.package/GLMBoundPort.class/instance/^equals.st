comparing
= anObject 
	"Compare this port with another for equality. 
 
	This is used to compare ParentPorts with PanePorts where the earlier is a proxy for the latter." 
	 
	^((anObject isKindOf: GLMBoundPort) and: [self pane = anObject pane]) 
		and: [self name = anObject name]