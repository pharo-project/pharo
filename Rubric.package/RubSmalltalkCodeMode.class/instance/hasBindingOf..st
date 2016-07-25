shout
hasBindingOf: aString
	" For the shout styler "
	^ self model notNil and: [ self model hasBindingOf: aString ]