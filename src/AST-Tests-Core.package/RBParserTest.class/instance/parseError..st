private
parseError: each
	RBParser 
		parseExpression: each first 
		onError: [ :string :pos | ^ self assert: pos = each last ].
	self assert: false description: 'Parser didn''t fail'