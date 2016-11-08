private
assertEnvironment: superEnvironment hasAllClassesInEnvironment: subEnvironment
	subEnvironment classesDo: [ :class |
		self assert: (superEnvironment includesClass: class) ]