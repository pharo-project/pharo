initialize-release
variable: aVariable value: expression from: encoder

	(aVariable isMemberOf: MessageAsTempNode)
		ifTrue: ["Case of remote temp vars"
				^ aVariable store: expression from: encoder].
	variable := aVariable.
	value := expression