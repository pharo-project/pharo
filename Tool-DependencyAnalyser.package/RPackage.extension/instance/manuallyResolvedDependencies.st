*Tool-DependencyAnalyser
manuallyResolvedDependencies
	| manifest |
	manifest :=  self classes
		detect: #isManifest
		ifNone: [ ^ #() ].
	^ manifest manuallyResolvedDependencies