*Tool-Base
inspectWithLabel: aLabel
	"Create and schedule an Inspector in which the user can examine the receiver's variables."
	^Smalltalk tools inspector inspect: self label: aLabel