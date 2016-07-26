testing
isRunningCogit
	"Returns true if we're running on the Cog JIT
	 (vmParameterAt: 46 is the size of the machine code zone)"

	^[(self parameterAt: 46) > 0] on: Error do:[:ex| ex return: false]