Class {
	#name : #CoCompletionEngineTest,
	#superclass : #CompletionEngineTest,
	#category : #'HeuristicCompletion-Tests'
}

{ #category : #running }
CoCompletionEngineTest >> newCompletionEngine [
	
	^ CoCompletionEngine new
]

{ #category : #asserting }
CoCompletionEngineTest >> shouldInheritSelectors [

	^ true
]

{ #category : #'test-interaction' }
CoCompletionEngineTest >> testInitialCompletionEngineHasNoContext [

	"If we did no interaction, no completion context should be initialized"
	
	| text |
	text := 'self mEthOdThatDoesNotExist'.
	self
		setEditorText: text;
		selectAt: text size + 1.
		
	self deny: controller hasCompletionContext.
]

{ #category : #'test-interaction' }
CoCompletionEngineTest >> testLeftWithoutResultsBroadensSelection [
	
	| text |
	text := 'self mEthOdThatDoesNotExist'.
	self
		setEditorText: text;
		selectAt: text size + 1.
	
	editor textArea openInWorld.
	controller openMenu.
	
	editor keystroke: (self keyboardEventFor: Character arrowLeft).
	
	self assert: controller context completionToken equals: 'mEthOdThatDoesNotExis'
]

{ #category : #'test-interaction' }
CoCompletionEngineTest >> testLeftWithoutResultsDoesNotCloseContext [
	
	| text firstContext |
	text := 'self mEthOdThatDoesNotExist'.
	self
		setEditorText: text;
		selectAt: text size + 1.
	
	editor textArea openInWorld.
	controller openMenu.
	firstContext := controller context.
	
	editor keystroke: (self keyboardEventFor: Character arrowLeft).
	
	self assert: controller context equals: firstContext
]

{ #category : #'test-interaction' }
CoCompletionEngineTest >> testOpenMenuCreatesCompletionContext [
	
	| text |
	text := 'self mEthOdThatDoesNotExist'.
	self
		setEditorText: text;
		selectAt: text size + 1.
	
	editor textArea openInWorld.
	controller openMenu.
	
	self assert: controller hasCompletionContext
]

{ #category : #'test-interaction' }
CoCompletionEngineTest >> testTypeCharacterWithoutResultsDoesNotCloseContext [
	
	| text firstContext |
	text := 'self mEthOdThatDoesNotExist'.
	self
		setEditorText: text;
		selectAt: text size + 1.
	
	editor textArea openInWorld.
	controller openMenu.
	firstContext := controller context.
	
	editor keystroke: (self keyboardEventFor: $a).
	
	self assert: controller context equals: firstContext
]

{ #category : #'test-interaction' }
CoCompletionEngineTest >> testTypeCharacterWithoutResultsNarrowsSelection [
	
	| text |
	text := 'self mEthOdThatDoesNotExist'.
	self
		setEditorText: text;
		selectAt: text size + 1.
	
	editor textArea openInWorld.
	controller openMenu.
	
	editor keystroke: (self keyboardEventFor: $a).
	
	self assert: controller context completionToken equals: 'mEthOdThatDoesNotExist' , 'a'
]
