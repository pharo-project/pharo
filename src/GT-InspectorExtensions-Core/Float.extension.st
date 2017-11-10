Extension { #name : #Float }

{ #category : #'*GT-InspectorExtensions-Core' }
Float >> gtInspectorFloatIn: composite [
	<gtInspectorPresentationOrder: 30>
	^ composite table
		title: 'Float';
		display: [ 
			(self respondsTo: #binaryLiteralString)
				ifTrue: [
					{'binary' -> self binaryLiteralString.
					'significand' -> self significand.
					'exponent' -> self exponent} ]
				ifFalse: [
					{'significand' -> self significand.
					'exponent' -> self exponent} ] ];
		column: 'Key' evaluated: #key;
		column: 'Value' evaluated: #value;
		send: #value
]

{ #category : #'*GT-InspectorExtensions-Core' }
Float >> gtInspectorVariableNodesIn: aCollection [
	
	aCollection
		add: (GTInspectorDynamicNode hostObject: self label: '1' value: (self at: 1));
		add: (GTInspectorDynamicNode hostObject: self label: '2' value: (self at: 2))
]
