private
assertEnvironment: superEnvironment hasAllSelectorsInEnvironment: subEnvironment
	subEnvironment classesDo: [ :class | 
		(subEnvironment selectorsForClass: class) 
			do: [ :selector |
				self assert: (superEnvironment includesSelector: selector in: class) ] ]