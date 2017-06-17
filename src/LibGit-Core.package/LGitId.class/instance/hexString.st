printing
hexString
	| string |
	self isExternal 
		ifFalse: [ ^handle hex ].
	string := String new: 40.
	string pin.
	self
		oid_fmt: string
		id: self.	
	string unpin.
	^ string