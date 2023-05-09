"
Utility class to easily check  dependencies of packages.


DADependencyChecker new dependenciesOf: #CargoPackageManager.

DADependencyChecker new transitiveDependenciesOf: #'CargoPackageManager-Minimal'.

DADependencyChecker new dependentsOf: #CargoPackageManager.
"
Class {
	#name : #DADependencyChecker,
	#superclass : #Object,
	#instVars : [
		'report',
		'shouldComputeMessageSendDependencies'
	],
	#category : #'Tool-DependencyAnalyser-Report'
}

{ #category : #private }
DADependencyChecker >> buildPackageRelationGraphFor: aPackageName [
	^ [ (DAPackageRelationGraph onPackagesNamed: { aPackageName })
		 	shouldComputeMessageSendDependencies: self shouldComputeMessageSendDependencies;
			build ]
		on: DAPotentialOutDatedDependencyWarning
		do: [ :ex | report addWarning: ex. ex resume ]
]

{ #category : #private }
DADependencyChecker >> computeDependenciesOf: aPackageName [
	| dependencies |
	dependencies := (self daPackageFor: aPackageName) dependentPackages collect: [:each | each packageName].
	^ ((dependencies
			addAll: (self manuallyResolvedDependenciesOf: aPackageName);
			yourself)
		difference: (self ignoredDependenciesOf: aPackageName))
		sorted
]

{ #category : #private }
DADependencyChecker >> computeDependenciesOf: aPackageName visitedPackages: visited [
	| packageName |
	packageName := aPackageName asSymbol.
	report
			add: packageName
			dependencies: (self computeDependenciesOf: packageName).
	visited add: packageName
]

{ #category : #'computing - dependencies' }
DADependencyChecker >> computeImageDependencies [

	| visited |
	self newReport.
	report
		analysisRunString: 'Pharo image dependencies';
		imageVersion: SystemVersion current.
	visited := Set new.
	Smalltalk organization packageNames do: [ :packageName | self computeDependenciesOf: packageName visitedPackages: visited ].
	^ report
]

{ #category : #private }
DADependencyChecker >> computeTransitiveDependenciesOf: aPackageName visitedPackages: visited [
	(visited includes: aPackageName) ifTrue: [ ^ self ].
	self computeDependenciesOf: aPackageName visitedPackages: visited.
	(self dependenciesOf: aPackageName)
		do: [ :each | self computeTransitiveDependenciesOf: each visitedPackages: visited ]
]

{ #category : #private }
DADependencyChecker >> daPackageFor: aPackageName [
	^ (((self buildPackageRelationGraphFor: aPackageName) packages)
			select: #isSeen) first
]

{ #category : #accessing }
DADependencyChecker >> dependenciesOf: aPackageName [
	^ report
		dependenciesOf: aPackageName asSymbol
		ifAbsentPut: [ self computeDependenciesOf: aPackageName ]
]

{ #category : #private }
DADependencyChecker >> ignoredDependenciesOf: aPackageName [
	^ (RPackage organizer packageNamed: aPackageName) ignoredDependencies
]

{ #category : #initialization }
DADependencyChecker >> initialize [
	super initialize.
	self newReport
]

{ #category : #private }
DADependencyChecker >> manuallyResolvedDependenciesOf: aPackageName [
	^ (RPackage organizer packageNamed: aPackageName) manuallyResolvedDependencies
]

{ #category : #initialization }
DADependencyChecker >> newReport [
	^ report := DADependencyReport new
]

{ #category : #'computing - dependencies' }
DADependencyChecker >> shortestPathToPackageIntroducingDependency: dependencyPackageName startingFrom: aPackageName [
	"This method allows to easily spots a package dependency we want to avoid in the System.
	ex: DADependencyChecker new
			computeImageDependencies;
			shortestPathToPackageIntroducingDependency: #'Athens-Core' startingFrom: 'Monticello'.
	The answer will be the shortest path (one, not all) going from #Monticello to #'Athens-Core',
	showing dependencies that leads to the introduction of the  #'Athens-Core' dependency.
		=> #(#Monticello #Kernel #'System-Settings-Core' #'Morphic-Widgets-Windows' #'Rubric' #'Athens-Morphic' #'Athens-Core').
	Now, we know that System-Settings-Core brings the whole graphical environment with him and that we should avoid the dependency
	between #Kernel and #'System-Settings-Core'."
	| dijkstra |
	report ifNil: [ ^ self ].
	dijkstra := AIDijkstra new.
	dijkstra nodes: report dependencies keys.
	report dependenciesDo: [ :packageName :dependencies |
		dependencies do: [ :dependency |
			dijkstra edges: { packageName } from: [ :ignored | packageName ] to: [ :ignored | dependency ] ] ].
	dijkstra runFrom: aPackageName to: dependencyPackageName.
	^ dijkstra reconstructPath
]

{ #category : #accessing }
DADependencyChecker >> shouldComputeMessageSendDependencies [
	^ shouldComputeMessageSendDependencies ifNil: [ true ]
]

{ #category : #accessing }
DADependencyChecker >> shouldComputeMessageSendDependencies: aBoolean [
	shouldComputeMessageSendDependencies := aBoolean 
]

{ #category : #'computing - dependencies' }
DADependencyChecker >> transitiveDependenciesOf: aPackageName [
	self newReport.
	report
		analysisRunString: 'Transitive dependencies of ' , aPackageName;
		imageVersion: SystemVersion current.
	self computeTransitiveDependenciesOf: aPackageName visitedPackages: Set new.
	^ report
]

{ #category : #accessing }
DADependencyChecker >> unresolvedDependenciesOf: aPackageName [
	^ [ (DAMessageSendAnalyzer on: aPackageName) missingMethodsWithPotentialMatchAfterManuallyResolvedDependenciesAddition
		reject: #isEmpty ]
		on: DAPotentialOutDatedDependencyWarning
		do: [ :ex | ex resume ]
]
