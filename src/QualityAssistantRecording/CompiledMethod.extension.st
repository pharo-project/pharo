Extension { #name : #CompiledMethod }

{ #category : #'*qualityAssistantRecording' }
CompiledMethod >> dataForQAR [

	| data |
	
	data := super dataForQAR
		at: #class put: self class name;
		at: #name put: self selector obfuscatedIfNecessary;
		yourself.
		
	self methodClass ifNotNil: [ :class |
		data at: #methodClass put: class dataForQAR ].
	
	Privacy sendSourceCode ifTrue: [ 
		data at: #sourceCode put: self sourceCode ].

	^ data
]
