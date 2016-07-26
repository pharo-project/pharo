building presentations
setUpCodeIn: aComposite

	aComposite pharoMethod
		title: [ self setUpPresentationTitle ];
		format: [ self session setUpMethod sourceCode ];
		smalltalkClass: [  self session testObject class ];
		when: [ self session overridesDefaultSetUpMethod ];
		doItReceiver: [ self session testObject ].