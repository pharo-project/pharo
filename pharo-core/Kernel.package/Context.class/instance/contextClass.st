accessing
contextClass
"The context class of a message send should be the one of the method to be evaluated, because if that method has some super sends, the method lookup won't work as expected'"

	^self method methodClass