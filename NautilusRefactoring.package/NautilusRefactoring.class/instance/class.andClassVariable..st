display
class: aClass andClassVariable: aBlock
	| variables index variable class |
	variables := aClass theNonMetaClass allClassVarNames asArray sort.
	index := self chooseFrom: variables.
	variable := variables at: index.
	class := aClass theNonMetaClass whichClassDefinesClassVar: variable.
	^ aBlock value: class value: variable