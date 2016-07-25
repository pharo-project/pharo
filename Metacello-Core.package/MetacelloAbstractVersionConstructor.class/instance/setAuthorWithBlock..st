api callbacks
setAuthorWithBlock: aBlock

	| spec |
	(spec := self root getAuthor) == nil
		ifTrue: [ 
			spec := self project valueHolderSpec.
			self root setAuthor: spec ].
	self with: spec during: aBlock