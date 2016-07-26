comparing
literalEqual: other

	^ (super literalEqual: other) and: [ self isZero not or: [ self sign = other sign ] ]