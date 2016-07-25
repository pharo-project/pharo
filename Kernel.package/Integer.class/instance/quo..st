arithmetic
quo: aNumber 
	"Refer to the comment in Number quo: "
	| ng quo |
	aNumber isInteger ifTrue: 
		[ng := self negative == aNumber negative == false.
		quo := (self digitDiv: aNumber neg: ng) at: 1.
		^ quo normalize].
	^ aNumber adaptToInteger: self andSend: #quo: