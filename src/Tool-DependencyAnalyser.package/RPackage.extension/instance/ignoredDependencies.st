*Tool-DependencyAnalyser
ignoredDependencies
	| manifest |
	manifest :=  self classes
		detect: #isManifest
		ifNone: [ ^ #() ].
	^ manifest ignoredDependencies