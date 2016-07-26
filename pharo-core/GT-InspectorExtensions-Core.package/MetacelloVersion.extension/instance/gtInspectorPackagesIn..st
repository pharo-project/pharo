*GT-InspectorExtensions-Core
gtInspectorPackagesIn: composite
	<gtInspectorPresentationOrder: 40>
	composite list
		title: 'Packages';
		display: [ (self spec packages packageSpecsInLoadOrderFor: self spec) select: [ :each | 
				each isKindOf: MetacelloPackageSpec ] ]