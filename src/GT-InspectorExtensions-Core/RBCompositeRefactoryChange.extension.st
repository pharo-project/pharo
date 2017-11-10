Extension { #name : #RBCompositeRefactoryChange }

{ #category : #'*GT-InspectorExtensions-Core' }
RBCompositeRefactoryChange >> gtInspectorPreviewIn: composite [
	composite list 
		title: 'Changes';
		display: [ self changes ];
		when: [ self changes notEmpty ]
]
