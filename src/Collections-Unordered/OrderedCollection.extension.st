Extension { #name : 'OrderedCollection' }

{ #category : '*Collections-Unordered' }
OrderedCollection >> removeDuplicates [
	"Remove the copies of elements, but keep the same order"

	"#(7 42 7 42 9) asOrderedCollection removeDuplicates asArray >>> #(7 42 9)"
	"#(1 2 3) asOrderedCollection removeDuplicates asArray >>> #(1 2 3)"

	| iterator seen |
	self ifEmpty: [ ^ self ].
	iterator := 1.
	seen := Set new.
	[ iterator <= self size ]
		whileTrue: [ | each |
			each := self at: iterator.
			(seen includes: each)
				ifTrue: [ self removeAt: iterator ]
				ifFalse: [ seen add: each. iterator := iterator + 1. ].
	 ]
]
