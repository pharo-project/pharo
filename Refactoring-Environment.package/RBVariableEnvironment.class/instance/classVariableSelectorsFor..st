private
classVariableSelectorsFor: aClass 
	| selectors classVars |
	selectors := Set new.
	classVars := Set new.
	classVariables keysDo: 
			[:each | 
			| cls |
			cls := self classForName: each.
			(cls notNil and: [aClass theNonMetaClass includesBehavior: cls]) 
				ifTrue: [classVars addAll: (classVariables at: each)]].
	classVars do: 
			[:each | 
			| binding |
			binding := aClass bindingOf: each.
			binding notNil 
				ifTrue: [selectors addAll: (aClass whichSelectorsReferTo: binding)]].
	^selectors