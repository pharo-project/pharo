accessing-classes
classes
	| classes |
	classes := IdentitySet new: 4096.
	self classesDo: [ :each | classes add: each ].
	^ classes