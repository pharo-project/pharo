private
defaultQueue
	defaultQueue ifNil: [defaultQueue := OrderedCollection new].
	^ defaultQueue