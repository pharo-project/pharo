temporary-patch
fixCreateDisplayScreen

	"To make OSPlatform initialize work when it is reloaded from monticello"
	Object subclass: #DisplayScreen
		instanceVariableNames: ''
		classVariableNames: ''
		category: 'Graphics-Display Objects'.
	(SmalltalkImage classPool at: #ShutDownList)
		add: #DisplayScreen after: #Delay.

	Object class compile: 'flushEvents'.