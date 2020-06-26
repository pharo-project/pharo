Extension { #name : #MetaLink }

{ #category : #'*GT-InspectorExtensions-Core' }
MetaLink >> gtInspectorDefinitonIn: composite [
	<gtInspectorPresentationOrder: 40>
	composite text
 		title: ['Definition' translated];
 		display: [self definitionString]
]

{ #category : #'*GT-InspectorExtensions-Core' }
MetaLink >> gtInspectorMethodsIn: composite [
	<gtInspectorPresentationOrder: 30>
	composite list
		title: [ 'Methods' translated ];
		display: [ (self methods collect: [:each | each asRingDefinition]) 
						sorted: [ :x :y | x selector < y selector ] ];
		showOnly: 30
]

{ #category : #'*GT-InspectorExtensions-Core' }
MetaLink >> gtInspectorNodesIn: composite [
	<gtInspectorPresentationOrder: 30>

	composite list 
		title: ['Nodes' translated];
		display: [ self nodes ];
		showOnly: 30
]
