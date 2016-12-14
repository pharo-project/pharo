running
installMethodsInBehaviorDefinition: behaviorDefinition prefixed: aPrefix
	| behaviorMirror  |
	behaviorMirror := bootstrapEnvironment at: behaviorDefinition name.
"	1haltIf: [ behaviorMirror isKindOf: EPTraitMirror ] ."
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