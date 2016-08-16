private
combineDependencies: aPackage
	| grouped composite |
	grouped := aPackage dependencies
		groupedBy: [ :each | each target ]
		having: [ :each | true ].
	
	aPackage clearDependencies.
	
	grouped keysAndValuesDo: [ :target :dependencies |
		composite := DACompositeDependency from: aPackage to: target.
		dependencies do: [ :each | composite add: each ].
		aPackage add: composite ]