Extension { #name : #Form }

{ #category : #'*GT-InspectorExtensions-Core' }
Form >> gtInspectorFormIn: composite [
	<gtInspectorPresentationOrder: 90>  
	^ composite morph 
		title: 'Morph';
		display: [ self asMorph ]
]
