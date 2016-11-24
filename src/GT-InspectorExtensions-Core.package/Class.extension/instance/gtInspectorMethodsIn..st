*GT-InspectorExtensions-Core
gtInspectorMethodsIn: composite
	"This provides a list of all methods provided by the current class"

	<localClassMethod> 
    "This method is supposed to be local in Class because of a good reason.
    We use this pragma to test if Class does not contain some accidental 
    local selectors."

	<gtInspectorPresentationOrder: 20>
	composite list 
		title: 'Methods';
		display: [ (self methods collect: #asRingDefinition) 
						sorted: [ :x :y | x selector < y selector ] ];
		format: #selector;
		tags: [ :each | {each methodClass name} ]