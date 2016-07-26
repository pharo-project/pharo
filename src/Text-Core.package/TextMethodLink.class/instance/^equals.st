comparing
= anotherObject

	^super = anotherObject 
		and: [ selector = anotherObject selector 
			and: [ browseSenders = anotherObject browseSenders ] ]