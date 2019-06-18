"
I am abstract command which subclasses should switch browser to particular trait hierarchy mode.
They should implement two methods: 

- requiredQueryResult
- traitScopeClass

Note that while scope and result in that cases are use same type of trait relation between classes they still provide different concerns and can be merged in single entity:
- required result is responsible to build trait hierarhcy from given set of classes 
- trat scope is responsible to retrieve trait related classes from given scope
"
Class {
	#name : #ClySwitchTraitHierarchyModeCommand,
	#superclass : #ClySwitchClassViewModeCommand,
	#category : #'Calypso-SystemPlugins-Traits-Browser'
}

{ #category : #testing }
ClySwitchTraitHierarchyModeCommand class >> isAbstract [
	^self = ClySwitchTraitHierarchyModeCommand 
]

{ #category : #execution }
ClySwitchTraitHierarchyModeCommand >> execute [
	| classScope |
	classScope := browser classSelection asItemsScope: ClyClassScope.
	
	browser 
		switchClassViewTo: self requiredQueryResult
		inScope: classScope, (classScope asScope: self traitScopeClass)
]

{ #category : #testing }
ClySwitchTraitHierarchyModeCommand >> isAppliedToBrowser [

	^browser classView showsItemsFromScope: self traitScopeClass
]

{ #category : #execution }
ClySwitchTraitHierarchyModeCommand >> requiredQueryResult [
	self subclassResponsibility 
]

{ #category : #execution }
ClySwitchTraitHierarchyModeCommand >> traitScopeClass [
	self subclassResponsibility 
]
