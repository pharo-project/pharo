tests - options
testLinkoptionInlineMetaObject
	| sendNode |
	sendNode := (ReflectivityExamples>>#exampleMethod) sendNodes first.
	link := MetaLink new 
		metaObject: self; 
		options: #(- optionInlineMetaObject);
		selector: #tagExec.
	sendNode link: link.
	self assert: sendNode hasMetalink.
		self assert: (ReflectivityExamples>>#exampleMethod) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: (ReflectivityExamples new exampleMethod = 5).
	self assert: (tag = #yes).
	"if we now change the meta object, the method should not be invalidated"
	link metaObject: self class new.
	self assert: (ReflectivityExamples>>#exampleMethod) class = CompiledMethod.