*GT-SpotterExtensions-Core
spotterPreviewContentsIn: aComposite
	<spotterPreview: 20>
	
	self isFile ifFalse: [ ^ self ].
	aComposite text
		title: [ self gtDisplayString ]; 
		display: #yourself;
		entity: (self readStreamDo: [ :stream | 
					| result |
					result := [(stream next: 10000) asString]
						on: Error 
						do: [ (stream binary next: 10000) asString ].
					stream size > 10000
						ifTrue: [ result, '  ... truncated ...' ]
						ifFalse: [ result ] ])