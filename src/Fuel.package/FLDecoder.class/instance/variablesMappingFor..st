accessing
variablesMappingFor: aClass

	| variables |
	variables := FLVariablesMapping materializing: aClass from: self.
	
	migrations 
		select: [ :m | (m targetClass == aClass) or: [ aClass inheritsFrom: m targetClass ] ]
		thenDo: [ :m | m applyTo: variables ].
		
	^ variables
	