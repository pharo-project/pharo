computing - dependencies
shortestPathToPackageIntroducingDependency: dependencyPackageName startingFrom: aPackageName
	"This method allows to easily spots a package a dependency we want to avoid in the System.
	ex: DADependencyChecker new 
			computeImageDependencies;
			shortestPathToPackageIntroducingDependency: #'Athens-Core' startingFrom: 'Monticello'.
	The answer will be the shortest path (one, not all) going from #Monticello to #'Athens-Core',
	showing dependencies that leads to the introduction of the  #'Athens-Core' dependency.
		=> #(#Monticello #Kernel #'System-Settings' #'Morphic-Widgets-Windows' #'Rubric' #'Athens-Morphic' #'Athens-Core').
	Now, we know that System-Settings brings the whole graphical environment with him and that we should avoid the dependency 
	between #Kernel and #'System-Settings'."
	| dijkstra |
	report ifNil: [ ^ self ].
	dijkstra := MalDijkstra new.
	dijkstra nodes: report dependencies keys.
	report dependenciesDo: [ :packageName :dependencies |
		dependencies do: [ :dependency | 
			dijkstra edges: { packageName } from: [ :ignored | packageName ] to: [ :ignored | dependency ] ] ].
	dijkstra runFrom: aPackageName to: dependencyPackageName.
	^ dijkstra backtrack