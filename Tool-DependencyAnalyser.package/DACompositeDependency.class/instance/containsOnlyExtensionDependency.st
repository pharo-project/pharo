testing
containsOnlyExtensionDependency
	(self dependencies isEmpty ) ifTrue: [ ^ false ].
	 ^ self dependencies allSatisfy: [ :dependency | dependency isExtensionDependency ]