*GT-InspectorExtensions-Core
gtInspectorImplementorsIn: composite
	<gtInspectorPresentationOrder: 30>
	composite list 
		title: 'Implementors';
		display: [ self systemNavigation allImplementorsOf: self ];
		when: [ (self systemNavigation allImplementorsOf: self) notEmpty ];
		showOnly: 30