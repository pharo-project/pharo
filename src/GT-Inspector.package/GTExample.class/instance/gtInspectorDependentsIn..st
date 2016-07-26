ui
gtInspectorDependentsIn: composite
	<gtInspectorPresentationOrder: 3>
	self dependents ifEmpty: [ ^ self ].
	^ (self dependents gtInspectorItemsIn: composite)
			title: 'Dependents'