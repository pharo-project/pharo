instance creation
parents: anInteger
	| path |
	path := self new: anInteger.
	1 to: anInteger do: [:i | path at: i put: '..'].
	^ path