running
installMethodsInBehaviorDefinition: behaviorDefinition prefixed: aPrefix
	| behaviorMirror  |
	behaviorMirror := bootstrapEnvironment at: behaviorDefinition name.
	self
		installMethodsInBehaviorMapping: behaviorDefinition
		using: self methodBuilder
		onMirror: behaviorMirror
		prefixed: aPrefix.
	self
		installMethodsInBehaviorMapping: behaviorDefinition classSide
		using: self methodBuilder
		onMirror: behaviorMirror basicClass
		prefixed: aPrefix.