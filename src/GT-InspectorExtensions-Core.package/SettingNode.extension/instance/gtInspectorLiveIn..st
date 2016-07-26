*GT-InspectorExtensions-Core
gtInspectorLiveIn: composite
	<gtInspectorPresentationOrder: 0>
	composite morph
		title: 'Live';
		display: [ self settingDeclaration inputWidget ];
		when: [ self settingDeclaration notNil and: [ self settingDeclaration inputWidget notNil ] ]