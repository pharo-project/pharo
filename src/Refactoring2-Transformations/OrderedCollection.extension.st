Extension { #name : #OrderedCollection }

{ #category : #'*Refactoring2-Transformations' }
OrderedCollection >> removeDuplicates [
	| iterator |
	"Remove the copies of elements, but keep the same order"
	
	self ifEmpty: [ ^ self ].
	iterator := 1.
	[ iterator <= self size ]
		whileTrue: [ | each newIndex |
			each := self at: iterator.			
			[ newIndex := (self indexOf: each startingAt: iterator+1).
			newIndex > 0 ]
				whileTrue: [ self removeAt: newIndex ].
			iterator := iterator + 1.
	 ]
]
