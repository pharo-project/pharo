scripting actions
onChangeOfPort: aPortName act: aBlock
	self portChangeActions add: (
		GLMPortChangeAction new 
			portName: aPortName;
			action: aBlock)