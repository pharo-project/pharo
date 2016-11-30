private
isMethodAlreadyLinked: aMethod
	^ aMethod notNil
		and: [ link methods anySatisfy: [ :m | m selector = aMethod selector and: [ m methodClass = aMethod methodClass ] ] ]