*GT-SpotterExtensions-Core
spotterImplementorsFor: aStep
	<spotterOrder: 30>
	self flag: #specialFilter.
	aStep listProcessor
		title: 'Implementors';
		filter: GTFilterImplementor item: [ :filter :context | 
			SystemNavigation default allBehaviorsDo: [ :class | class localMethods do: filter ] ];
		keyBinding: $m meta;
		wantsToDisplayOnEmptyQuery: false