updating
colorText: aText
	table dataSource numberOfRows ~= 0
		ifTrue: [ aText makeAllColor: self table theme textColor ]
		ifFalse: [ aText addAttribute: (TextColor new color: Color red) ].
	^ aText