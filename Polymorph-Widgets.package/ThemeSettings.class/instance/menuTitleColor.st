menu
menuTitleColor
	"Answer the value of menuTitleColor"

	Display depth = 1 ifTrue: [^ Color white].
	Display depth = 2 ifTrue: [^ Color gray].
	^menuTitleColor 
		ifNil: [self menuColor 
				ifNil: [Color gray] 
				ifNotNil: [:mc | self preferGradientFill 
					ifTrue: [mc]
					ifFalse: [mc twiceDarker]]]