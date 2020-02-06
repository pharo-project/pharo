Extension { #name : #CompiledMethod }

{ #category : #'*GT-InspectorExtensions-Core' }
CompiledMethod >> gtDisplayOn: aStream [
	aStream print: self methodClass; nextPutAll: '>>'; store: self selector.
]

{ #category : #'*GT-InspectorExtensions-Core' }
CompiledMethod >> gtInspectorSourceIn: composite [
	<gtInspectorPresentationOrder: 00>
	^ composite pharoMethod 
		title: 'Source';
		smalltalkClass: [ self methodClass ];
		display: [ self sourceCode ];
		act: [ :text | self methodClass compile: text text notifying: nil ]
			icon: GLMUIThemeExtraIcons glamorousAccept
			on: $s
			entitled: 'Accept';
		act: [ self browse ] 
			icon: GLMUIThemeExtraIcons glamorousBrowse 
			entitled: 'Browse'
]
