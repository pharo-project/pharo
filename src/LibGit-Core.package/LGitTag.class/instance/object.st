accesing
object
	| peeled |
	peeled := LGitObject of: self repository.
	self tag_peel: peeled.
	^ peeled concreteObject