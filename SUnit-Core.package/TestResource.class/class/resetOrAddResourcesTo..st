private
resetOrAddResourcesTo: aCollection
	"Add correctly set-up resources to the collection unless already there. Reset any imperfectly-set-up resources, so current isNil will return true if they are re-encountered via an indirectly self-prerequing resource;  circular references cannot be set up so will never reply true to isAlreadyAvailable, but may have correctly-set-up prereqs to add and/or imperfectly-set-up ones to reset, so do not abort the loop first time round."

	current ifNil: [^self].
	self isAlreadyAvailable
		ifFalse:
			[self reset.
			self resources do: [:each | each resetOrAddResourcesTo: aCollection]]
		ifTrue:
			[(aCollection includes: self)
				ifFalse:
					[self resources do: [:each | each resetOrAddResourcesTo: aCollection].
					aCollection add: self]]

"The cloned 'self resources do: ...' line in both blocks is, I think, the best way to write this method so that its logic is clear.  The first loop resets this resource immediately, before traversing its resources;  the second traverses before adding"