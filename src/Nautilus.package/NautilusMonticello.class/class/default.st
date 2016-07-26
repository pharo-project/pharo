singleton
default

	^ Default ifNil: [ Default := self basicNew initialize ]