private
computeNodeIdentifier
	"Compute and return a 'node identifier', a 16 element byte array that should be unique to this image, vm, invocation, network, machine combination"
	
	| nodeData |
	nodeData := String streamContents: [ :out |
		Smalltalk at: #NetNameResolver ifPresent: [ :netNameResolver |
			out print: netNameResolver localHostAddress.
			out print: netNameResolver localHostName].
		out print: SystemVersion current.
		out print: Smalltalk vm imagePath.
		out print: Smalltalk commandLine options.
		out print: SessionManager default currentSession hash.
		out print: self hash ].
	^ (MD5 hashMessage: nodeData) hash asByteArrayOfSize: 4