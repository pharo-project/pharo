Extension { #name : #OCInstanceScope }

{ #category : #'*OpalCompiler-Tools' }
OCInstanceScope >> gtInspectorSourceCodeIn: composite [
	<gtInspectorPresentationOrder: 30> 

	^ composite text 
		title: 'Source';
		display: [ '"instance scope, no source"']
]
