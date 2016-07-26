private
copySelector: aSelector ofMethod: aCompiledMethod toClass: aClass
	aClass
		addAndClassifySelector: aSelector
		withMethod: aCompiledMethod copy
		inProtocol: self extensionProtocolName
		notifying: nil