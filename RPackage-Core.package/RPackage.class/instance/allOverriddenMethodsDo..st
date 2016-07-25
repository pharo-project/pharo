system compatibility
allOverriddenMethodsDo: aBlock
	"Evaluates aBlock with all the overridden methods in the system"
	^ ProtoObject withAllSubclassesDo: [:class | 
		self overriddenMethodsInClass: class do: aBlock]
