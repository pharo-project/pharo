Extension { #name : #ByteArray }

{ #category : #'*GT-InspectorExtensions-Core' }
ByteArray >> gtInspectorBytesIn: composite [
	<gtInspectorPresentationOrder: 30>
	^ composite table
		title: 'Bytes';
		display: [ 
			(self size < 2048 or: [ self confirm: 'Looking at more than 2048 bytes might be slow. Continue ?' ])
				ifTrue: [ {
					'hex string' -> (String streamContents: [ :out | 
						self do: [ :each | 
							each printOn: out base: 16 nDigits: 2 ] ]).
					'hex lines' -> (String streamContents: [ :out |
						self withIndexDo: [ :each :index |
							index = 1 ifFalse: [ 
								index - 1 \\ 8 = 0 ifTrue: [ out cr ] ifFalse: [ out space ] ].
								each printOn: out base: 16 nDigits: 2 ] ]).
					'latin1' -> ([ ZnCharacterEncoder latin1 decodeBytes: self ]
											on: ZnCharacterEncodingError do: [ :exception | exception ]).
					'utf8' -> ([ ZnUTF8Encoder new decodeBytes: self ] 
											on: ZnCharacterEncodingError do: [ :exception | exception ]).
					'integer' -> self asInteger.
					'integer reversed' -> self reversed asInteger } ]
				ifFalse: [ #() ] ];
		column: 'Key' evaluated: #key;
		column: 'Value' evaluated: #value;
		send: #value
]
