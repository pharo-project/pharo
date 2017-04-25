*GT-InspectorExtensions-Core
gtInspectorItemsIn: composite
	^ composite fastTable
		title: 'Items';
		display: [ self ];
		useCache;
		beMultiple;
		enableElementIndex;
		wantsAutomaticRefresh: true;
		column: 'Index' evaluated: [ :value :idex | idex asString ] width: 50;
		column: 'Item' evaluated: [ :value | GTObjectPrinter asTruncatedTextFrom: value ];
		send: [ :result | 
			result isNil
				ifTrue: [ nil ]
				ifFalse: [ result value size = 1 
					ifTrue: [result anyOne value]  
					ifFalse: [self species withAll: (result collect: [:each | each value])]]]
		"withSmalltalkSearch;
		showOnly: 50;
		helpMessage: 'Quick selection field. Given your INPUT, it executes: self select: [:each | INPUT ]'."