Extension { #name : #CompiledBlock }

{ #category : #'*GT-InspectorExtensions-Core' }
CompiledBlock >> gtDisplayOn: stream [
	self printOn: stream
]

{ #category : #'*GT-InspectorExtensions-Core' }
CompiledBlock >> gtInspectorSourceIn: composite [
	<gtInspectorPresentationOrder: 30>
	^ composite pharoMethod
		title: 'Source';
		smalltalkClass: [ self methodClass ];
		display: [ self sourceCode ];
		act: [ self browse ]
			icon: GLMUIThemeExtraIcons glamorousBrowse
			entitled: 'Browse'
]
