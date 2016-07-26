system simulation
runSimulated: aBlock contextAtEachStep: block2
	"Simulate the execution of the argument, aBlock, until it ends. aBlock 
	MUST NOT contain an '^'. Evaluate block2 with the current context 
	prior to each instruction executed. Answer the simulated value of aBlock."
	| current returnContext exception |
	
	aBlock hasMethodReturn
		ifTrue: [ self error: 'simulation of blocks with ^ can run loose' ].
		
	current := [ aBlock
					on: Exception
					do: [ :ex | SimulationExceptionWrapper signalForException: ex  ] ] asContext.
	
	returnContext := Context
			sender: nil
			receiver: self home receiver
			method: self home method
			arguments: self home arguments.
	
	current pushArgs: Array new from: returnContext.
	
	[current == returnContext]
		whileFalse: [ 
			block2 value: current.
			current := current step ].
	
	exception := returnContext pop.
	exception class == SimulationExceptionWrapper
		ifTrue: [ ^ exception exception signal ].
	^ exception
	