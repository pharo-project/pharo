*qualityAssistantRecording
dataForQAR
		
	| data |
	
	data := super dataForQAR
		at: #class put: self class name;
		at: #name put: self selector obfuscatedIfNecessary;
		yourself.
		
	self methodForQAR ifNotNil: [ :method |
		data at: #method put: method dataForQAR ].
	
	Privacy sendSourceCode ifTrue: [ 
		data at: #sourceCode put: self sourceCode ].

	^ data