class-initialization
initializeClasses
	self runInitializeOnClasses: self classesInitializationOrder.
	UIManager default inform: 'initialization done!'