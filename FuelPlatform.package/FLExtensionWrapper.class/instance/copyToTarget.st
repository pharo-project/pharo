private
copyToTarget
	self targetClass
		addAndClassifySelector: self selector
		withMethod: self method copy
		inProtocol: self extensionProtocolName
		notifying: nil