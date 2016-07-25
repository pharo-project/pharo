testing-environments
testProtocolEnvironment
	| aProtocolEnvironment size |
	aProtocolEnvironment := RBProtocolEnvironment 
				onEnvironment: RBBrowserEnvironment new
				class: Object
				protocols: #(#printing #testing).
	self universalTestFor: aProtocolEnvironment.
	self 
		assert: (aProtocolEnvironment implementorsOf: #printString) numberSelectors 
				= 1.
	size := 0.
	aProtocolEnvironment classesDo: [:each | size := size + 1].
	self assert: size = 1.
	aProtocolEnvironment selectorsForClass: Object
		do: 
			[:each | 
			self assert: (#(#printing #testing) 
						includes: (RBBrowserEnvironment new whichProtocolIncludes: each in: Object))]