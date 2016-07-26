private
universalTestFor: aBrowserEnvironment 
	self labelFor: aBrowserEnvironment.
	self uniqueClassesIn: aBrowserEnvironment.
	self numberSelectorsFor: aBrowserEnvironment.
	self storeStringFor: aBrowserEnvironment.
	self classNamesFor: aBrowserEnvironment.
	self copyFor: aBrowserEnvironment.
	self categoriesFor: aBrowserEnvironment.
	self protocolsFor: aBrowserEnvironment.
	self classesFor: aBrowserEnvironment.
	self keysFor: aBrowserEnvironment.
	self assert: aBrowserEnvironment problemCount = 0 = aBrowserEnvironment isEmpty