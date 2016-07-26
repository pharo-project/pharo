private
isFailToken: anObject
	^ (self objectClass: anObject) == Array 
			and: [ anObject size = 2 and: [(anObject at: 1) == PrimitiveFailToken]]