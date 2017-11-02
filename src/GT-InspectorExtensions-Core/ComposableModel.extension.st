Extension { #name : #ComposableModel }

{ #category : #'*GT-InspectorExtensions-Core' }
ComposableModel >> gtInspectorModelNestingIn: composite [
	<gtInspectorPresentationOrder: 20>
	composite tree 
		title: 'Nesting';
		display: [ :root | { 'root' -> self } ];
		children: [ :each | 
			each value class allInstVarNames 
				collect: [ :iv | iv->(each value instVarNamed: iv) ]
				thenSelect: [ :association | 
					(association value isKindOf: ComposableModel) 
						and: [ (#('owner' 'window') includes: association key) not ] ] ];
		format: [ :association | association key , ': ' , association value asString ];
		send: #value
]

{ #category : #'*GT-InspectorExtensions-Core' }
ComposableModel >> gtInspectorPreviewIn: composite [
	<gtInspectorPresentationOrder: 30>
	
	self widget ifNotNil: [ :w | w gtInspectorPreviewIn: composite ]
]
