accessing
allowedBounds

	^ allowedBounds ifNil: [ allowedBounds := self globalBounds ]