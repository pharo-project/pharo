private
compactIfNeeded
	| newModelCache |
	self models size > 250 ifFalse: [ ^ self ].
	newModelCache := self models select: [ :each | each notNil ].
	modelCache := newModelCache
	