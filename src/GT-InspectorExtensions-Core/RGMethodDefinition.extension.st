Extension { #name : #RGMethodDefinition }

{ #category : #'*GT-InspectorExtensions-Core' }
RGMethodDefinition >> gtInspectorASTIn: composite [
	<gtInspectorPresentationOrder: 35> 
	(GTSimpleRBTreeBrowser new treeIn: composite)
		title: 'AST';
		display: [ :anObject | {anObject ast} ]
]

{ #category : #'*GT-InspectorExtensions-Core' }
RGMethodDefinition >> gtInspectorBytecodeIn: composite [
	<gtInspectorPresentationOrder: 30> 
	^ (GTBytecodeBrowser new treeIn: composite)
		title: 'Bytecode'
]

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
