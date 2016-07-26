tests - instead
testInsteadSendMultipleSites
	| sendNodes instance |
	self skip.
	sendNodes := (ReflectivityExamples >> #exampleMethodMultipleSites) sendNodes.
	link := MetaLink new
		metaObject: [:receiver :selector :arguments | receiver perform: selector withArguments: arguments];
		selector: #value:value:value:;
		control: #instead;
		arguments: #(#receiver #selector #arguments).
	sendNodes do: [ :sn | sn link: link].
	self assert: sendNodes anyOne hasMetalink.
	self assert: (ReflectivityExamples >> #exampleMethodMultipleSites) class = ReflectiveMethod.
	instance := ReflectivityExamples new.
	self assert: instance exampleMethodMultipleSites= 5.