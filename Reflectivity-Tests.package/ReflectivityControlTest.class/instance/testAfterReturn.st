tests - after
testAfterReturn
	| returnNode |
	returnNode := (ReflectivityExamples >> #exampleMethod) ast statements first.
	self assert: returnNode isReturn.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec;
		control: #after.
	self should: [returnNode link: link] raise: Error.
	