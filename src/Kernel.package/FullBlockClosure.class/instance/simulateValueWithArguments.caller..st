simulation
simulateValueWithArguments: anArray caller: aContext
	"Simulate the valueWithArguments: primitive. Fail if anArray is not an array of the right arity."
	| newContext |
	newContext := (Context newForMethod: self compiledBlock)
						setSender: aContext
						receiver: receiver
						method: self compiledBlock
						closure: self
						startpc: self compiledBlock initialPC.
	((newContext objectClass: anArray) ~~ Array
	 or: [numArgs ~= anArray size]) ifTrue:
		[^Context primitiveFailTokenFor: nil].
	newContext stackp: self compiledBlock numTemps.
	1 to: numArgs do:
		[:i| newContext at: i put: (anArray at: i)].
	1 to: self basicSize do:
		[:i| newContext at: i + numArgs put: (self at: i)].
	^newContext