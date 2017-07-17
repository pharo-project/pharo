dependencies
dependentClasses
	"Return the list of classes used myself"
	
	"Morph dependentClasses"
	| cll | 
	cll := Set new.
	"A class depends on its superclass"
	self superclass ifNotNil: [ cll add: self superclass ].
	
	"We unify a class and its metaclass"
	(self methods, self theMetaClass methods) 
		do: [ :m | m literalsDo: [ :l | 
					"We also check if the method is not an extension"
					((((l isVariableBinding 
					and: [ l value notNil ]) 
					and: [ l value isBehavior ])
					and: [ m category notEmpty ] )
					and: [ m category first ~= $* ])  ifTrue: [ cll add: l value ] ]  ].
	^ cll asArray