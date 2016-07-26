protocol
actionOnGenerateGraphViz
	| graphvizGenerator |
	buttonGenerateGraphViz
		action: [  
			self ifGraphVizAbsent: [ self error: 'Please install Graphviz Support' ].
			graphvizGenerator := DAPackageRelationGraphGenerator newWith: self relationGraph named: 'PackageDependencyGraph'.
			self saveGraphAndOpenWith: graphvizGenerator ]