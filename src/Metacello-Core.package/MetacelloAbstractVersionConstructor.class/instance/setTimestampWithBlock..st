api callbacks
setTimestampWithBlock: aBlock

	| spec |
	(spec := self root getTimestamp) == nil
		ifTrue: [ 
			spec := self project valueHolderSpec.
			self root setTimestamp: spec ].
	self with: spec during: aBlock