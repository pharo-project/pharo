building presentations
decompiledCodeWidgetIn: composite forContext: aContext

	^ composite pharoMethod
		title: 'Decompiled';
		format: [ aContext method codeForNoSource ];
		smalltalkClass: [  aContext methodClass ];
		doItReceiver: [ aContext receiver ];
		doItContext: [  aContext ]