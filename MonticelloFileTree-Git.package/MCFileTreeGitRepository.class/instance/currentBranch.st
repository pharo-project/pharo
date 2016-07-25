accessing
currentBranch
	"How do we handle a detached head status?"

	| branches indexOfStar current |
	branches := self gitBranchs.
	indexOfStar := branches indexOf: '*'.
	current := branches at: indexOfStar + 1.
	(current first = $( and: [ current last ~= $) ])
		ifTrue: [ 
			(branches allButFirst: indexOfStar + 1)
				do: [ :each | 
					current := current , ' ' , each.
					each last = $)
						ifTrue: [ ^ current ] ].
			^ current ]
		ifFalse: [ ^ current ]