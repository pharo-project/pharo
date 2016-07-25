testing
isRunningCog
	"Returns true if we're running on a Cog VM (JIT or StackInterpreter)
	 (parameterAt: 42 is the number of stack pages)"

	^[(self parameterAt: 42) > 0] on: Error do:[:ex| ex return: false]