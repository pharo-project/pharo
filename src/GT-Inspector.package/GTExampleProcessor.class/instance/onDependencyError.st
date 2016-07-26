accessing
onDependencyError
	^ onDependencyError ifNil: [ onDependencyError := self defaultOnDependencyError ]