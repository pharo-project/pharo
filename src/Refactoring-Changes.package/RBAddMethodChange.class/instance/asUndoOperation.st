converting
asUndoOperation
	^ (self changeClass includesSelector: self selector)
		ifTrue: [ 
			| oldProtocol |
			oldProtocol := RBBrowserEnvironment new whichProtocolIncludes: self selector in: self changeClass.
			oldProtocol isNil
				ifTrue: [ oldProtocol := #accessing ].
			RBAddMethodChange compile: (self methodSourceFor: self selector) in: self changeClass classified: oldProtocol ]
		ifFalse: [ RBRemoveMethodChange remove: selector from: self changeClass ]