initialize-release
jsonMode: boolean
	(jsonMode := boolean)
		ifTrue: [ 
			self
				escape: $' with: #pass;
				escape: $" with: '\"' ]
		ifFalse: [ 
			self
				escape: $" with: #pass;
				escape: $' with: '\''' ]