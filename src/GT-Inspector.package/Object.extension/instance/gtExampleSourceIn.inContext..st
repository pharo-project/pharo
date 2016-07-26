*GT-Inspector-Examples-UI
gtExampleSourceIn: composite inContext: aGTInspector
	<gtInspectorPresentationOrder: 100>
	| previousRawSelection |
	"we first check to see if we have at least two panes, because only
	if we have two panes does it make sense to check the raw selection of the previous pane"
	((aGTInspector isKindOf: GTInspector) and: [aGTInspector first panes size >= 2]) ifFalse: [ ^ self ].
	
	previousRawSelection := (aGTInspector first panes reverse second port: #rawSelection) value.
	previousRawSelection isGTExample ifTrue: [ 
		previousRawSelection gtInspectorSourceIn: composite ]