tests - options
testLinkoptionInlineCondition
	| sendNode |
	sendNode := (ReflectivityExamples>>#exampleMethod) sendNodes first.
	link := MetaLink new 
		metaObject: self; 
		options: #(- optionInlineCondition);
		condition: [ true ];
		selector: #tagExec.
	sendNode link: link.
	self assert: sendNode hasMetalink.
		self assert: (ReflectivityExamples>>#exampleMethod) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: (ReflectivityExamples new exampleMethod = 5).
	self assert: (tag = #yes).
	"if we now change the meta object, the method should not be invalidated"
	link condition: [true].
	self assert: (ReflectivityExamples>>#exampleMethod) class = CompiledMethod.
	tag := nil.
	self assert: (ReflectivityExamples new exampleMethod = 5).
	self deny: tag isNil.
	link condition: [true].
	self assert: (ReflectivityExamples>>#exampleMethod) class = CompiledMethod.
	tag := nil.
	link condition: [false].
	self assert: (ReflectivityExamples new exampleMethod = 5).
	self assert: tag isNil.