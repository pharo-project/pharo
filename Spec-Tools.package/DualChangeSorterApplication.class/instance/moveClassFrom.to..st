menu - class
moveClassFrom: src to: dest

	self checkThatSidesDiffer: [^ self].
	(self okToChange and: [ src selectedClass notNil]) ifFalse: [ ^ self ].

	self copyClassFrom: src to: dest.
	src forgetClass