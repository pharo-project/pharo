accessing
selectorsForClass: aClass 
	| selectors |
	selectors := IdentitySet new.
	self selectorsForClass: aClass do: [ :each | selectors add: each ].
	^ selectors