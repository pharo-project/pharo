display
class: aClass andInstVariable: aBlock
	| variables index variable class |
	variables := aClass instVarNames sorted.
	variables ifEmpty:[ ^ self notifyNoVariablesIn: aClass].
	index := self chooseFrom: variables.
	variable := variables at: index.
	class := aClass whichClassDefinesInstVar: variable.
	class := class isMeta not
				ifTrue: [ RBClass existingNamed: class name ] 
				ifFalse: [ RBMetaclass existingNamed: class theNonMetaClass name ].
	class model: environment.
	^ aBlock value: class value: variable