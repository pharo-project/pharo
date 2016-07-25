mimic enumerating
do: aBlock
	" the iterator must keep an internal collection of items being iterated. This guarantees that the iteration is not done multiple times in case somebody sends multiple subsequent messages like #size, #items or #first:. This will slightly decrease performance while iterating "
	items 
		ifNotNil: [ 
			items do: aBlock ]
		ifNil: [ 
			items := self defaultItems.
			self itemDo: [ :each | 
				items add: each.
				aBlock value: each ] ]