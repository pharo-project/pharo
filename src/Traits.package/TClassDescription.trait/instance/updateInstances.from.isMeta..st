initialize-release
updateInstances: oldInstances from: oldClass isMeta: isMeta
	"Recreate any existing instances of the argument, oldClass, as instances of the receiver,
	 which is a newly changed class. Permute variables as necessary, and forward old instances
	 to new instances.  Answer nil to defeat old clients that expect an array of old instances.
	 The old behaviour, which necessitated a global GC, exchanged identities and answered
	 the old instances.  But no clients used the result.  This way we avoid the unnecessary GC,"

	| variable instSize newInstances |
	oldInstances isEmpty
		ifTrue: [ ^ nil ].	"no instances to convert"
	isMeta ifTrue:
		[(oldInstances size = 1
		  and: [self soleInstance class == self
				or: [self soleInstance class == oldClass]]) ifFalse:
			[^self error: 'Metaclasses can only have one instance']].
	variable := self isVariable.
	instSize := self instSize.
	newInstances := Array new: oldInstances size.
	1 to: oldInstances size do: [ :i | newInstances at: i put: (self newInstanceFrom: (oldInstances at: i) variable: variable size: instSize) ].	"Now perform a bulk mutation of old instances into new ones"
	oldInstances elementsForwardIdentityTo: newInstances.
	^ nil