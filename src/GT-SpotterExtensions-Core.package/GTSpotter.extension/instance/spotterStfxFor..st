*GT-SpotterExtensions-Core
spotterStfxFor: aStep
	<spotterOrder: 30>
	self flag: #specialFilter.
	^ aStep listProcessor
		title: 'ws.stfx.eu';
		filter: GTNullFilter item: [ :filter :context | 
			(context textTrimmed beginsWith: 'http://ws.stfx.eu')
				ifTrue: [ GTPlayPage loadFromPublishUrl: context textTrimmed thenDo: filter ] ]