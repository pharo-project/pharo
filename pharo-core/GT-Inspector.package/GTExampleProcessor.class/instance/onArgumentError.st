accessing
onArgumentError
	^ onArgumentError ifNil: [ onArgumentError := self defaultOnArgumentError ]