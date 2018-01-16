Extension { #name : #RG2MethodDefinition }

{ #category : #'*GT-InspectorExtensions-Core' }
RG2MethodDefinition >> gtInspectorImplementorsIn: composite [
	<gtInspectorPresentationOrder: 4>
	composite list 
		title: 'Implementors';
		display: [ self compiledMethod implementors ]
]

{ #category : #'*GT-InspectorExtensions-Core' }
RG2MethodDefinition >> gtInspectorSendersIn: composite [
	<gtInspectorPresentationOrder: 2>
	composite list 
		title: 'Senders';
		display: [ self compiledMethod senders ]
]

{ #category : #'*GT-InspectorExtensions-Core' }
RG2MethodDefinition >> gtInspectorSourceIn: composite [
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
