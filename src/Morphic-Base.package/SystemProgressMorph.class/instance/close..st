private
close: aSystemProgressItemMorph

	self bars isEmpty ifTrue: [ ^ self ].
	
	lock critical: [
		aSystemProgressItemMorph delete.
		self bars size = 0 ifTrue: [ 
			self width: 0.
			self delete ] ].

	self refresh.