*GT-InspectorExtensions-Core
gtInspectorSendersIn: composite
	<gtInspectorPresentationOrder: 30>
	composite list 
		title: 'Senders';
		display: [ self systemNavigation allSendersOf: self ];
		when: [ (self systemNavigation allSendersOf: self) notEmpty ];
		showOnly: 30;
		withSmalltalkSearch 