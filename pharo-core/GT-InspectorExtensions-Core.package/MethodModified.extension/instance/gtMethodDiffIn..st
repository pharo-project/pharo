*GT-InspectorExtensions-Core
gtMethodDiffIn: aComposite
	<gtInspectorPresentationOrder: 40>
	aComposite diff
		title: 'Diff';
		display: [ 
			Array 
				with: self newMethod sourceCode
				with: self oldMethod sourceCode ]
