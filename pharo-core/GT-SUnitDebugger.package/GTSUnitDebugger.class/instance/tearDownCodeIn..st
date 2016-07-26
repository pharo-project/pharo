building presentations
tearDownCodeIn: aComposite

	aComposite pharoMethod
		title: [ self tearDownPresentationTitle ];
		format: [ self session tearDownMethod sourceCode ];
		smalltalkClass: [  self session testObject class ];
		when: [ self session overridesDefaultTearDownMethod ];
		doItReceiver: [ self session testObject ].