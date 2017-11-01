Extension { #name : #Character }

{ #category : #'*GT-InspectorExtensions-Core' }
Character >> gtInspectorCharacterIn: composite [
	<gtInspectorPresentationOrder: 30>
	^ composite table
		title: 'Character';
		display: [
			{'self' -> self.
			'codepoint' -> self codePoint.
			'unicode' -> (String streamContents: [ :stream | 
				stream << 'U+'.
				self codePoint printOn: stream base: 16 nDigits: 4 ])} ];
		column: 'Key' evaluated: #key;
		column: 'Value' evaluated: [ :each | each value printString ];
		send: #value
]
