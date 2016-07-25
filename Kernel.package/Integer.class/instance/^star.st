arithmetic
* aNumber
	"Refer to the comment in Number * " 
	aNumber isInteger ifTrue:
		[^ self digitMultiply: aNumber 
					neg: self negative ~~ aNumber negative].
	^ aNumber adaptToInteger: self andSend: #*