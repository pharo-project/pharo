Extension { #name : #OCClassScope }

{ #category : #'*OpalTools' }
OCClassScope >> gtInspectorSourceCodeIn: composite [
	<gtInspectorPresentationOrder: 30> 

	^ composite text 
		title: 'Source';
		display: [ '"class scope, no source"']
]
