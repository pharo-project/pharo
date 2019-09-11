Extension { #name : #ClassDescription }

{ #category : #'*Deprecated80' }
ClassDescription >> classThatDefinesInstanceVariable: instVarName [
	self 
		deprecated: 'use #classThatDefinesInstVarNamed:' 
		transformWith: '`@receiver classThatDefinesInstanceVariable: `@statement' -> '`@receiver classThatDefinesInstVarNamed: `@statement'.
	
	^self classThatDefinesInstVarNamed: instVarName
]

{ #category : #'*Deprecated80' }
ClassDescription >> instVarNameForIndex: index [
	"Answer the named instance variable with index index or nil if none."
	"we deprecate this as it is not useful know that we have meta objects for variables"

	| slot |
	self deprecated: 'Please use the slot API directly'.
	slot := self classLayout allSlots 
		detect: [ :each | each isVirtual not and: [ each index = index ] ] ifNone: [^nil].
	^slot name
]
