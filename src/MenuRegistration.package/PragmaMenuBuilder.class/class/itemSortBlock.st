private
itemSortBlock
	"The block which is used to sort a menu tree"
	^ [:a :b | 
		((a order notNil and: [b order notNil]) and: [a order ~= b order])
			ifTrue: [a order < b order]
			ifFalse: [((a order isNil and: [b order isNil]) or: [a order = b order])
				ifTrue: [true]
				ifFalse: [a order ifNil: [false] ifNotNil: [true]]]]