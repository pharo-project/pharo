menu morph
isMenuEmpty
	^menuMorph isNil or: [ self context model notNil and: [ self context model isEmpty ] ]