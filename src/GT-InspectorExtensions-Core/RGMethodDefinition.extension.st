Extension { #name : #RGMethodDefinition }

{ #category : #'*GT-InspectorExtensions-Core' }
RGMethodDefinition >> gtInspectorImplementorsIn: composite [
	<gtInspectorPresentationOrder: 4>
	composite list 
		title: 'Implementors';
		display: [ self compiledMethod implementors ]
]

{ #category : #'*GT-InspectorExtensions-Core' }
RGMethodDefinition >> gtInspectorSendersIn: composite [
	<gtInspectorPresentationOrder: 2>
	composite list 
		title: 'Senders';
		display: [ self compiledMethod senders ]
]

{ #category : #'*GT-InspectorExtensions-Core' }
RGMethodDefinition >> gtInspectorSourceIn: composite [
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
