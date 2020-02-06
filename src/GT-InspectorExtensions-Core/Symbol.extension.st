Extension { #name : #Symbol }

{ #category : #'*GT-InspectorExtensions-Core' }
Symbol >> gtInspectorImplementorsIn: composite [
	<gtInspectorPresentationOrder: 30>
	composite list 
		title: 'Implementors';
		display: [ self systemNavigation allImplementorsOf: self ];
		format: #name;
		showOnly: 30
]

{ #category : #'*GT-InspectorExtensions-Core' }
Symbol >> gtInspectorSendersIn: composite [
	<gtInspectorPresentationOrder: 30>
	composite list 
		title: 'Senders';
		display: [ self systemNavigation allSendersOf: self ];
		format: #name;
		showOnly: 30 
]
