Extension { #name : 'MetacelloAbstractVersionConstructor' }

{ #category : '*Deprecated14' }
MetacelloAbstractVersionConstructor >> author: aBlockOrString [

	self deprecated: 'The author metadata is not supported anymore. It was used with SmalltalkHub but not this information is already managed by git or other CVS.
	If you wish to keep this as a trace, you can leave the infos in a method comment.'
]

{ #category : '*Deprecated14' }
MetacelloAbstractVersionConstructor >> configuration: aString with: aBlock [

	self error: 'Configurations are not supported anymore, only baseline can be used as dependencies.'
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
