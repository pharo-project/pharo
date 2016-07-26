private
orderAssignBlock
	"The block which is used to set MenuRegistration tree node order inst var (which is used to sort the tree)"

	^ [ :list | 
	list
		detect: [ :n | n order notNil ]
		ifFound: [ :firstWithOrder | 
			| idx order |
			"A menu registration with an order indication hase been found - then compute order of previous and next nodes"
			idx := list indexOf: firstWithOrder.
			order := firstWithOrder order.
			idx > 1
				ifTrue: [ 
					idx - 1 to: 1 do: [ :pos | 
						(list at: pos) order: order - 1.
						order := order - 1 ] ].
			order := firstWithOrder order.
			idx + 1 to: list size do: [ :pos | 
				(list at: pos) order ifNil: [ (list at: pos) order: order + 1 ] ifNotNil: [ order := (list at: pos) order ].
				order := order + 1 ].
			list ]
		ifNone: [ list ]	"No order has been set - do not touch anything, the list order is ok" ]