adding/removing
removeRule: aRule

	(rules anySatisfy: [ :rule | rule name = aRule name] )
		ifTrue: [rules := rules reject:  [ :rule | rule name = aRule name]]
		 ifFalse: [rules do: [ :rule |
				rule isComposite 
					ifTrue: [rule removeRule: aRule]]] 