*GT-InspectorExtensions-Core
gtInspectorAllReferencesIn: composite
	"This provides a list of all methods provided by the current class"
	<gtInspectorPresentationOrder: 20>
	composite list 
		title: 'All Ref';
		display: [ (SystemNavigation default allReferencesTo: self binding) 
						sorted: [ :x :y | x name < y name ] ];
		format: #name;
		tags: [ :each | { each package name } ]