*GT-InspectorExtensions-Core
gtInspectorHeadersIn: composite 
	<gtInspectorPresentationOrder: 30>
	^ (self headers gtInspectorHeadersIn: composite) 
		title: [:anObject | self headers headers size asString , ' headers' ]