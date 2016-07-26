building presentations
methodCodeWidgetIn: composite forContext: aContext

	^ composite pharoMethod
		title: 'Source';
		format: [ aContext sourceCode ];
		smalltalkClass: [  aContext methodClass ];
		doItReceiver: [ aContext receiver ];
		doItContext: [  aContext ]