accessing
yourself
	"Answer self. The message yourself deserves a bit of explanation.
	Sending yourself is handy to get the receiver of a message. It is especially useful when using a cascade since all the cascaded messages are send to the receiver. Let us look at an example.
	
	Imagine that you have the following expression
		#('hello' 'world') at: 2 put: 'pharo'
	and that you would like to get the modified array #('hello' 'pharo') - the receiver of the message at:put:. 
	
	Right now the expression #('hello' 'world') at: 2 put: 'pharo' returns 'pharo' and not the receiver because the message at:put: always returns the second argument.
	 
	To get the receiver we can simply use a cascade as follows:
		#('hello' 'world') at: 2 put: 'pharo'; yourself
	and since yourself returns the receiver of the message we will get the modified array.
	Stepping back using a cascade and yourself is equivalent to declare a variable, assign the expression and return the variable as follows:
	| a |
	a := #('hello' 'world') at: 2 put: 'pharo'.
	a 
	"
	
	^self