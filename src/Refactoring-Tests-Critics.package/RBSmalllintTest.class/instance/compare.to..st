private
compare: subEnvironment to: superEnvironment 
	subEnvironment classesDo: [ :class | 
		(subEnvironment selectorsForClass: class) 
			do: [ :selector | self assert: (superEnvironment includesSelector: selector in: class) ] ]