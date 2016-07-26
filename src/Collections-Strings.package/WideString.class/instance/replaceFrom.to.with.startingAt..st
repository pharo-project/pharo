accessing
replaceFrom: start to: stop with: replacement startingAt: repStart 

	<primitive: 105>
	replacement class == String ifTrue: [
		^ self replaceFrom: start to: stop with: (replacement asWideString) startingAt: repStart.
	]. 

	^ super replaceFrom: start to: stop with: replacement startingAt: repStart.
