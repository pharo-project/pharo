Extension { #name : #BlockClosure }

{ #category : #'*GT-InspectorExtensions-Core' }
BlockClosure >> gtInspectorSourceCodeIn: composite [ 
	<gtInspectorPresentationOrder: 40>
	^(self sourceNode gtInspectorSourceCodeIn: composite)
		doItReceiver: [ self receiver ];
		doItContext: [ self outerContext ]
]
