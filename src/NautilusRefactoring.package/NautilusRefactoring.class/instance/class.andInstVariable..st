display
class: aClass andInstVariable: aBlock
	| variables index variable class |
	variables := aClass instVarNames sorted.
	variables ifEmpty:[ ^ self notifyNoVariablesIn: aClass].
	index := self chooseFrom: variables.
	variable := variables at: index.
	class := aClass whichClassDefinesInstVar: variable.
	class := class isMeta not
				ifTrue: [ RBClassModelFactory rbClass existingNamed: class name ] 
				ifFalse: [ RBClassModelFactory rbMetaclass existingNamed: class theNonMetaClass name ].
	class model: environment.
	^ aBlock value: class value: variable