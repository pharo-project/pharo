*GT-InspectorExtensions-Core
gtInspectorAllReferencesOutsideIn: composite
	"This provides a list of all methods provided by the current class"
	<gtInspectorPresentationOrder: 20>
	composite list 
		title: 'All Ref Outside';
		display: [ ((SystemNavigation default allReferencesTo: self binding) 
							reject: [ :method | method package name == self package name ])
						sorted: [ :x :y | x name < y name ] ];
		format: #name;
		tags: [ :each | { each package name } ]