*GT-InspectorExtensions-Core
gtInspectorHeadersIn: composite 
	<gtInspectorPresentationOrder: 30>
	^ (self headers gtInspectorItemsIn: composite) 
		display: [ :anObject | self headers associations ];
		title: [ :anObject | self headers size asString , ' items' ]