accessing
onRecursionError
	^ onRecursionError ifNil: [ onRecursionError := self defaultOnRecursionError ]