temporary-patch
fixShutdownList
	"The default shutdown list comes with classes not in the bootstrap. Moreover, some classes in the boostrap need that some classes in the bootstrap are already registered in the list.
	This fix should be removed once we can use the new Session Manager."
	(SmalltalkImage classVariableNamed: 'ShutDownList')
		value: (OrderedCollection withAll: #(#Symbol #OSPlatform #Delay)).
