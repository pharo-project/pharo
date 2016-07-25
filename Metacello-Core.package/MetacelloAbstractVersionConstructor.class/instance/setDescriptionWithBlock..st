api callbacks
setDescriptionWithBlock: aBlock

	| spec |
	(spec := self root getDescription) == nil
		ifTrue: [ 
			spec := self project valueHolderSpec.
			self root setDescription: spec ].
	self with: spec during: aBlock