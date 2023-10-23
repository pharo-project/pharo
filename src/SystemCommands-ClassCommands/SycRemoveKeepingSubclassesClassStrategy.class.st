Class {
	#name : 'SycRemoveKeepingSubclassesClassStrategy',
	#superclass : 'SycRemoveClassStrategy',
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'testing' }
SycRemoveKeepingSubclassesClassStrategy class >> canExecuteWithReferences: hasReferences subclasses: hasSubclasses users: hasUsers [
	^hasSubclasses & hasReferences not & hasUsers not
]

{ #category : 'execution' }
SycRemoveKeepingSubclassesClassStrategy >> execute: aSycRemoveClassCommand [

	aSycRemoveClassCommand executeRefactoringsAs: RBRemoveClassPushingStateToSubclassesRefactoring
]

{ #category : 'testing' }
SycRemoveKeepingSubclassesClassStrategy >> isComplexRefactoring [
	"Class removals are always complex refactorings"
	^true
]

{ #category : 'execution' }
SycRemoveKeepingSubclassesClassStrategy >> orderForBrowser [
	^3
]

{ #category : 'execution' }
SycRemoveKeepingSubclassesClassStrategy >> userRequestString [
	^'Remove it, keeping subclasses'
]
