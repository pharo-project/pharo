accessing-classes
classNames
	| names |
	names := IdentitySet new: 4096.
	self classesDo: [ :each | names add: each theNonMetaClass name ].
	^ names