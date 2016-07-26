*GT-SpotterExtensions-Core
spotterForExpressionsFor: aStep
	<spotterOrder: 1>
	self flag: #specialFilter.
	aStep listProcessor
		title: 'Calculator';
		filter: GTNullFilter item: [ :filter :context | 
			self evaluateExpressionInContext: context usingFilter: filter ];
		wantsToDisplayOnEmptyQuery: false