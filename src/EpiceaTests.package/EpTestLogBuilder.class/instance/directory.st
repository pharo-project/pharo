accessing
directory

	^ directory ifNil: [ directory := self newDirectory ]