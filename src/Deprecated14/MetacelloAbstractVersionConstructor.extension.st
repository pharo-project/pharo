Extension { #name : 'MetacelloAbstractVersionConstructor' }

{ #category : '*Deprecated14' }
MetacelloAbstractVersionConstructor >> setAuthorWithBlock: aBlock [

	| spec |
	self deprecated: 'Author should be provided as a string and not a block since it brind not value.'.
	(spec := self root getAuthor) ifNil: [
		spec := self project valueHolderSpec.
		self root author: spec ].
	self with: spec during: aBlock
]

{ #category : '*Deprecated14' }
MetacelloAbstractVersionConstructor >> setBlessingWithBlock: aBlock [

	| spec |
	self deprecated: 'Blessing should be provided as a string and not a block since it brind not value.'.
	(spec := self root getBlessing) ifNil: [
		spec := self project valueHolderSpec.
		self root blessing: spec ].
	self with: spec during: aBlock
]

{ #category : '*Deprecated14' }
MetacelloAbstractVersionConstructor >> setDescriptionWithBlock: aBlock [

	| spec |
	self deprecated: 'Description should be provided as a string and not a block since it brind not value.'.
	(spec := self root getDescription) ifNil: [
		spec := self project valueHolderSpec.
		self root description: spec ].
	self with: spec during: aBlock
]

{ #category : '*Deprecated14' }
MetacelloAbstractVersionConstructor >> setTimestampWithBlock: aBlock [

	| spec |
	self deprecated: 'Timestamp should be provided as a string and not a block since it brind not value.'.
	(spec := self root getTimestamp) ifNil: [
		spec := self project valueHolderSpec.
		self root timestamp: spec ].
	self with: spec during: aBlock
]
