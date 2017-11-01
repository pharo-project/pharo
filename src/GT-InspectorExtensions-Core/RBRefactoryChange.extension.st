Extension { #name : #RBRefactoryChange }

{ #category : #'*GT-InspectorExtensions-Core' }
RBRefactoryChange >> gtInspectorPreviewIn: composite [
	<gtInspectorPresentationOrder: 0>
	composite diff
		title: 'Changes';
		display: [ { self oldVersionTextToDisplay . self textToDisplay } ]
]
