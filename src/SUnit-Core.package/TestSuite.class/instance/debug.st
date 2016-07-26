running
debug
	self setUp.
	[
		self tests do: [:each | 
					each debug.
					self announceTest: each.
					self changed: each.
		]
	] ensure:[ 
		self tearDown. 
	].

