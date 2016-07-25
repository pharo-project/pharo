layout
fullBounds
	self contents ifNil: [ self contents: 'String Morph' ].
	^super fullBounds