*GT-SpotterExtensions-Core
spotterConcreteMethodFor: aStep
    <spotterOrder: 30>
	self flag: #specialFilter.
	^ aStep listProcessor
		title: 'Methods';
		filter: GTNullFilter item: [ :filter :context |
			self concreteMethodsInContext: context usingFilter: filter ];
		wantsToDisplayOnEmptyQuery: false