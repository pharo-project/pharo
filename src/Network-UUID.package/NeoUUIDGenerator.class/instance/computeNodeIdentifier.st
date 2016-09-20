private
computeNodeIdentifier
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