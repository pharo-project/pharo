*GT-InspectorExtensions-Core
gtInspectorMorphIn: composite
	<gtInspectorPresentationOrder: 90>
	| morphPresentation |
	morphPresentation := composite morph.
	morphPresentation
		title: 'Morph';
		display: [ [ self imageForm asMorph
				on: #mouseDown
				send: #value:
				to:
					[ :event | morphPresentation selection: (self morphsAt: event position) ] ]
				on: Error
				do: [ IconicListItem
						text: 'Error drawing Morph'
						icon: (Smalltalk ui icons iconNamed: #smallWarningIcon) ] ];
		act: [ :pres | self exportAsPNG ] entitled: 'Export as PNG';
		act: [ :pres | self delete ] entitled: 'Delete'