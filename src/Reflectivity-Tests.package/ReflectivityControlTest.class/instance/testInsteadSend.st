tests - instead
testInsteadSend
	| sendNode instance |
	sendNode := (ReflectivityExamples >> #exampleMethod) sendNodes first.
	link := MetaLink new
		metaObject: #receiver;
		selector: #perform:withArguments:;
		control: #instead;
		arguments: #(#selector #arguments).
	sendNode link: link.
	self assert: sendNode hasMetalink.
	self assert: (ReflectivityExamples >> #exampleMethod) class = ReflectiveMethod.
	instance := ReflectivityExamples new.
	self assert: instance exampleMethod = 5.