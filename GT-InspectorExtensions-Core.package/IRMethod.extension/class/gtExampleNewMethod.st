*GT-InspectorExtensions-Core
gtExampleNewMethod
	<gtExample>
	^ IRBuilder new 
		addTemps: #(a);
		pushReceiver;
		pushLiteral: 1;
		storeTemp: #a;
		ir