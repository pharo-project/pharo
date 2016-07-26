api callbacks
setBlessingWithBlock: aBlock

	| spec |
	(spec := self root getBlessing) == nil
		ifTrue: [ 
			spec := self project valueHolderSpec.
			self root setBlessing: spec ].
	self with: spec during: aBlock