converting
asUndoOperation
	^ (self changeClass includesSelector: self selector)
		ifTrue: [ 
			| oldProtocol |
			oldProtocol := RBBrowserEnvironment new whichProtocolIncludes: self selector in: self changeClass.
			oldProtocol isNil
				ifTrue: [ oldProtocol := #accessing ].
			RBRefactoryChangeManager changeFactory addMethodSource: (self methodSourceFor: self selector) in: self changeClass classified: oldProtocol ]
		ifFalse: [ RBRefactoryChangeManager changeFactory removeMethod: selector from: self changeClass ]