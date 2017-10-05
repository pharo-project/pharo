comparing
compareClass: oldClass with: aBuilder
	| changes |
	self builder: aBuilder.
	
	changes := self compareClass.
	
	changes do:[:e | aBuilder addChange: e].