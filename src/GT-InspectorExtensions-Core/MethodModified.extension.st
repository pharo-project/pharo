Extension { #name : #MethodModified }

{ #category : #'*GT-InspectorExtensions-Core' }
MethodModified >> gtMethodDiffIn: aComposite [
	<gtInspectorPresentationOrder: 40>
	aComposite diff
		title: 'Diff';
		display: [ 
			Array 
				with: self newMethod sourceCode
				with: self oldMethod sourceCode ]

]
