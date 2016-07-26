accessing
searchStrategies
	^ SearchStrategies ifNil: [ SearchStrategies := self defaultSearchStrategies ]