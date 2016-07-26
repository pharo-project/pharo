accessing
origin
	self properties 
		at: #traitSource 
		ifPresent: [ :traitMethod| ^ traitMethod origin].
	^ self methodClass traitOrClassOfSelector: self selector