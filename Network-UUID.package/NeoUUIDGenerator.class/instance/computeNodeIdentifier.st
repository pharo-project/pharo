private
computeNodeIdentifier
	| nodeData |
	nodeData := String streamContents: [ :out |
		out print: NetNameResolver localHostAddress.
		out print: NetNameResolver localHostName.
		out print: SystemVersion current.
		out print: Smalltalk vm imagePath.
		out print: Smalltalk commandLine options.
		out print: SessionManager default currentSession hash.
		out print: self hash ].
	^ (MD5 hashMessage: nodeData) hash asByteArrayOfSize: 4