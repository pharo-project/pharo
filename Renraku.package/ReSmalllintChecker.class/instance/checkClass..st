private
checkClass: aClass

	(environment includesClass: aClass) ifFalse: [ ^ self ].

	self getCritiquesAbout: aClass by: classRules