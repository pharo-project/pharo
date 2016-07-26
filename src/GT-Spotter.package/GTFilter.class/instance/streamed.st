accessing
streamed
	^ streamed ifNil: [ streamed := self defaultStreamed ]