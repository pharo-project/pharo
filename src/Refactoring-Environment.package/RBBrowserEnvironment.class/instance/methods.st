accessing
methods
	| methods |
	methods := IdentitySet new: 4096.
	self methodsDo: [ :each | methods add: each ].
	^ methods