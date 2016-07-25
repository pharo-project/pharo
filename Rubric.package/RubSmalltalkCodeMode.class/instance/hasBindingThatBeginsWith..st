shout
hasBindingThatBeginsWith: aString 
	" For the shout styler "
	^ self model notNil and: [ self model hasBindingThatBeginsWith: aString ]