Usage: perform <global> <messageSelector>  [ <arguments> ] [ --save ]
	global	A class name or a global name. Receiver of the message (e.g. Smalltalk)
	messageSelector   The message selector to be perfomed
	
Documentation:
Performs a message on a given class or global object. This command-line handler can be used to execute some code if a compiler is not loaded in the image. All message arguments are strings.

Examples:
	pharo Pharo.image perform Smalltalk garbageCollect --save
	pharo Pharo.image perform PerformMessageCommandLineHandler printReversed:times:  olleH 10
