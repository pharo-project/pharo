*GT-InspectorExtensions-Core
gtInspectorMethodsIn: composite
	"This provides a list of all methods provided by the current class"
	<gtInspectorPresentationOrder: 20>
	composite list 
		title: 'Methods';
		display: [ (self methods collect: #asRingDefinition) 
						sorted: [ :x :y | x selector < y selector ] ];
		format: #selector;
		tags: [ :each | {each methodClass name} ]