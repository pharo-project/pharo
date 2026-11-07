"
I am a command to categorize all unclassified methods of given classes
"
Class {
	#name : 'SycCategorizeAllUnclassifiedMethodsCommand',
	#superclass : 'SycClassCommand',
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'execution' }
SycCategorizeAllUnclassifiedMethodsCommand >> executeRefactoring [
	"MethodClassifier should not be hardcoded and should be based on Smalltalk tools"

	classes do: [ :each | each uncategorizedSelectors do: [ :selector | MethodClassifier classify: each >> selector ] ]
]

{ #category : 'accessing' }
SycCategorizeAllUnclassifiedMethodsCommand >> iconName [
	^#browse
]

{ #category : 'testing' }
SycCategorizeAllUnclassifiedMethodsCommand >> isAvailableInMenuOf: aToolContext [

	^ aToolContext isFullBrowserMethodGroupContext
]

{ #category : 'accessing' }
SycCategorizeAllUnclassifiedMethodsCommand >> name [
	^'Categorize all uncategorized'
]

{ #category : 'accessing' }
SycCategorizeAllUnclassifiedMethodsCommand >> order [

	^ 10000
]

{ #category : 'execution' }
SycCategorizeAllUnclassifiedMethodsCommand >> prepareFullExecution [

	super prepareFullExecution.
	classes := classes collect: [ :each | self context currentMetaLevelOf: each ]
]
