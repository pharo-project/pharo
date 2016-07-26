queries
sourceSnapshotOf: aReference
	| ancestors reference |
	ancestors := aReference workingCopy ancestry ancestors.
	ancestors isEmpty ifTrue: [ ^ MCSnapshot new ].
	reference := GoferVersionReference name: ancestors first name.
	^ (reference resolveWith: self gofer) version snapshot