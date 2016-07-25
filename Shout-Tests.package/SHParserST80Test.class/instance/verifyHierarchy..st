utilities
verifyHierarchy: aBehavior
	aBehavior withAllSubclassesDo: [ :class | self verifyClass: class; verifyClass: class class ]