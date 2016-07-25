singleton
default
	^ Default ifNil: [ Default := self new ]