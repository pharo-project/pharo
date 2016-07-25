*Tool-Base
browseAllAccessesTo: instVarName from: aClass
	"Create and schedule a Message Set browser for all the receiver's methods 
	or any methods of a subclass/superclass that refer to the instance variable name."

	"self new browseAllAccessesTo: 'contents' from: Collection."

	| coll |
	coll := self allAccessesTo: instVarName from: aClass.
	^ self browseMessageList: coll name: 'Accesses to ' , instVarName autoSelect: instVarName refreshingBlock: [ :method | | index |
			index := aClass instVarIndexFor: instVarName.
			(method readsField: index)
				or: [ method writesField: index ] ]