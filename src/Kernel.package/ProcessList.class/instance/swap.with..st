accessing
swap: ix1 with: ix2
	"Reimplemented, super would create an infinite loop"
	| minIx maxIx link1Prev link2Prev link1 link2 link1Next link2Next newLink2Next |
	((self validIndex: ix1) and: [self validIndex: ix2])	ifFalse: [^ self errorOutOfBounds].
	
	"Get edge case out of the way"
	ix1 = ix2 ifTrue: [^ self ].
	
	"Sort indexes to make boundary-checks easier" 
	minIx := ix1 min: ix2.
	maxIx := ix2 max: ix1.
	
	link1Prev := (minIx = 1) ifFalse: [self linkAt: minIx -1].
	link1 := link1Prev ifNotNil: [ link1Prev nextLink]
				ifNil: [self linkAt: minIx].
	link1Next := link1 nextLink.
	link2Prev := self linkAt: maxIx -1.
	link2 := link2Prev nextLink.
	link2Next := link2 nextLink.
	
	"Link at start being swapped"
	link1 = firstLink ifTrue: [firstLink := link2.] ifFalse: [link1Prev nextLink: link2].
	"Link at end being swapped"
	link2 = lastLink ifTrue: [lastLink := link1] ifFalse: [].
	"Links  being swapped adjacent"
	newLink2Next := (link1 nextLink = link2) ifTrue: [link1] ifFalse: [link2Prev nextLink: link1.
		link1Next].
	link1 nextLink: link2Next.
	link2 nextLink: newLink2Next.
	