tests - options
testLinkOneShot
	| sendNode |
	sendNode := (ReflectivityExamples>>#exampleMethod) sendNodes first.
	link := MetaLink new 
		metaObject: self; 
		options: #(+ optionOneShot);
		selector: #tagExec.
	sendNode link: link.
	self assert: sendNode hasMetalink.
	self assert: tag isNil.
	self assert: (ReflectivityExamples new exampleMethod = 5).
	self assert: (tag = #yes).
	self deny: sendNode hasMetalink.
	tag := nil.
	self assert: (ReflectivityExamples new exampleMethod = 5).
	self assert: tag isNil.
