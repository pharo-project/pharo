comparing
= anObject

	^anObject class == self class and: [
		anObject hash = hash and: [
			anObject string = string ] ]