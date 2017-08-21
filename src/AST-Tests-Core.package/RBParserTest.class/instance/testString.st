tests parsing
testString
	self assert: (self parserClass parseMethod: 'selector ^ ''<''') isMethod.
	self assert: (self parserClass parseMethod: 'selector ^ ''<'', self') isMethod.
	self assert: (self parserClass parseMethod: 'selector ''=''') isMethod.
	self assert: (self parserClass parseMethod: 'selector '':=''') isMethod.
	self assert: (self parserClass parseMethod: 'selector ''<''') isMethod.
	self assert: (self parserClass parseMethod: 'selector ''>''') isMethod.
	self assert: (self parserClass parseMethod: 'selector ^ ''<>''') isMethod.