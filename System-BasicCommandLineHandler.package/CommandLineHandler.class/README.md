A CommandLineHandler is activated by the CommandLine. 

The responsible handler with the highest priority is selected and its instance-side method #activate is invoked.

By default the handlers are selected by their class name. In the following shell invocation the FooHandler is chosen:

	pharo Pharo.image FooHandler

A handler may provide a short name with the class-side #commandName method. If the FooHandler defined #commandName returning 'foo' it would be activated with the following shell invocation:
	
	pharo Pharo.image foo
	
For more sophisticated handler selection the CommandLineHandler should implement the #isResponsibleFor: class-side method. An instance of the current command line options is passed to this method which should then return a boolean.

Between all the responsible handlers the one with the highes #priority is chosen. To change the priority overwrite the class-side accessor.
