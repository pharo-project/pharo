*GT-InspectorExtensions-Core
gtInspectorFloatIn: composite
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