*GT-InspectorExtensions-Core
gtInspectorProjectsIn: composite
	<gtInspectorPresentationOrder: 40>
	composite list 
		title: 'Projects';
		display: [
			| projects |
			projects := OrderedCollection new.
			self
					projectDo: [:prj | projects add: prj ] 
					packageDo: [:pkg | ] 
					groupDo: [:ignored | ].
			projects ]
