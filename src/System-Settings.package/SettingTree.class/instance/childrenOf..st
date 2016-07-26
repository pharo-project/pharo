querying
childrenOf: aNode
	| children idx order |
	"Get aNode children. Assign children order if one child has an order which is not nil"
	children := self nodeList select: [ :p | p parentName = aNode settingDeclaration name ].
	children
		detect: [ :n | n order notNil ]
		ifFound: [ :firstWithOrder | 
			idx := children indexOf: firstWithOrder.
			order := firstWithOrder order.
			idx > 1
				ifTrue: [ 
					idx - 1 to: 1 do: [ :pos | 
						(children at: pos) order: order - 1.
						order := order - 1 ] ].
			order := firstWithOrder order.
			idx + 1 to: children size do: [ :pos | 
				(children at: pos) order ifNil: [ (children at: pos) order: order + 1 ] ifNotNil: [ order := (children at: pos) order ].
				order := order + 1 ] ].
	^ children