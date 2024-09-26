"
I am a command to categorize all unclassified methods of given classes
"
Class {
	#name : 'SycCategorizeAllUnclassifiedMethodsCommand',
	#superclass : 'SycClassCommand',
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'accessing' }
SycCategorizeAllUnclassifiedMethodsCommand >> defaultMenuIconName [
	^#glamorousBrowse
]

{ #category : 'accessing' }
SycCategorizeAllUnclassifiedMethodsCommand >> defaultMenuItemName [
	^'Categorize all uncategorized'
]

{ #category : 'execution' }
SycCategorizeAllUnclassifiedMethodsCommand >> execute [
	"MethodClassifier should not be hardcoded and should be based on Smalltalk tools"

	classes do: [ :each |
		each uncategorizedSelectors do: [ :selector |
			MethodClassifier classify: each >> selector ] ]
]

{ #category : 'execution' }
SycCategorizeAllUnclassifiedMethodsCommand >> prepareFullExecutionInContext: aToolContext [

	super prepareFullExecutionInContext: aToolContext.
	classes := classes collect: [:each | aToolContext currentMetaLevelOf: each]
]
