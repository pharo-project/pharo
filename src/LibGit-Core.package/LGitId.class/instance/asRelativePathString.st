printing
asRelativePathString
	| string |
	string := String new: 41.
	string pin.
	self
		oid_pathfmt: string
		id: self.
	string unpin.
	^ string