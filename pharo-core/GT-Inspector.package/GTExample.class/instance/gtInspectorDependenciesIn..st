ui
gtInspectorDependenciesIn: composite
	<gtInspectorPresentationOrder: 2>
	self arguments ifEmpty: [ ^ self ].
	^ ((self dependenciesOrErrors ifEmpty: [ ^ self ]) gtInspectorItemsIn: composite)
			title: 'Dependencies'