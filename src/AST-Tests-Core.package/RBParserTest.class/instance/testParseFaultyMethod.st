tests parsing
testParseFaultyMethod
	| node strangeExpressions |
	
	strangeExpressions := OrderedCollection new .
	strangeExpressions 
		add: 'method: asd   self ,';
		add: 'method: asd   self a:'; "message without needed argument"
		add: 'method: asd   []]'; 
		add: 'method: asd  [  ] ,';"binary message without second argument"
		add: 'method:  ^^'; "returning twice"
		add: 'method: asd  ['; "opening a block"
		add: 'method: asd  ^ asd. n'; "invalid expresion after return"
		add: 'method: asd  ^ {'; "Only Open a literal array"
		add: 'selector '''; 
		add: 'selector #^';
		add: 'selector ¿'.


	strangeExpressions do: [ :exp | 
			node := self parseFaultyMethod: exp.
			self assert: node isMethod.
			self assert: node isFaulty.
	].

	