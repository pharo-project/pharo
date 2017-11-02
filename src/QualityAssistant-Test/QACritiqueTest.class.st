Class {
	#name : #QACritiqueTest,
	#superclass : #TestCase,
	#category : 'QualityAssistant-Test'
}

{ #category : #running }
QACritiqueTest >> runWithDialogPopping: anAssertingBlock [
 
	UIManager default class = MorphicUIManager
		ifTrue: [ self runWithMorphicDialogPopping: anAssertingBlock ]
		ifFalse: anAssertingBlock

]

{ #category : #running }
QACritiqueTest >> runWithMorphicDialogPopping: anAssertingBlock [

	| semaphore worldSubmoprhs newMorphs closingProcess |

	semaphore := Semaphore new.

	worldSubmoprhs := World submorphs.
	closingProcess := [
		[ worldSubmoprhs = World submorphs ] whileTrue:  [ Processor yield ].
		newMorphs := (World submorphs difference: worldSubmoprhs).
		self assert: newMorphs size equals: 1. 
		newMorphs first close.
		semaphore signal ] fork.
	
	
	anAssertingBlock
		on: TestFailure
		do: [ :failure |
			closingProcess terminate.
			semaphore signal.
			failure pass. ].
		
	semaphore wait.
]

{ #category : #tests }
QACritiqueTest >> testFixAction [

	| crit action |
	
	"This tests should be rewritten: it only runs ok in graphical mode. In headless it creates sometimes new windows and sometimes it does not (depending if it uses UIManager of directly Nautilus)."
	self flag: #bug.
	self skip.
	
	crit := ReTransformationCritique
		for: thisContext method
		by: (RBTransformationRule allSubclasses detect: #isVisible) new
		tree: thisContext method parseTree.
	
	action := crit actions detect: [ :a | a description = 'Automatically resolve the issue' ].
	
	self runWithDialogPopping: [
		self
			shouldnt: [ action actOnCritic: crit ofEntity: crit sourceAnchor entity ]
			raise: Error ]
]

{ #category : #tests }
QACritiqueTest >> testGuidedBan [

	| crit |
	
	"This tests should be rewritten: it only runs ok in graphical mode. In headless it creates sometimes new windows and sometimes it does not (depending if it uses UIManager of directly Nautilus)."
	self flag: #bug.
	self skip.
	
	crit := ReTrivialCritique
		for: thisContext method
		by: (RBLintRule allSubclasses detect: #isVisible) new.
	
	self runWithDialogPopping: [
		self shouldnt: [ crit guidedBan ] raise: Error ]
	
]
