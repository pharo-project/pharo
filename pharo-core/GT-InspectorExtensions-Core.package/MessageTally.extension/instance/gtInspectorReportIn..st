*GT-InspectorExtensions-Core
gtInspectorReportIn: composite
	<gtInspectorPresentationOrder: 60>
	^ composite text
		title: 'Report';
		display: [ String streamContents: [:s | self report: s ] ]