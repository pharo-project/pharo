private
addVariable: name to: aClass
	name first isUppercase ifTrue: [ aClass addClassVarNamed: name] ifFalse: [aClass addInstVarNamed: name].