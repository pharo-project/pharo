Extension { #name : #Text }

{ #category : #'*GT-InspectorExtensions-Core' }
Text >> gtInspectorItemsIn: composite [
	"No items presentation for a Text"
]

{ #category : #'*GT-InspectorExtensions-Core' }
Text >> gtInspectorTextIn: composite [
	<gtInspectorPresentationOrder: -10>  
	 composite text
		title: 'Text';
		format: self
]
