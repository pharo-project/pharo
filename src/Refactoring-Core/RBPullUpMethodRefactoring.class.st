"
I am a refactoring for moving a method up to the superclass. 

My precondition verify that this method does not refere to instance variables not accessible in the superclass. And this method does not sends a super message that is defined in the superclass.
If the method already exists and the superclass is abstract or not referenced anywhere, replace that implementation and push down the old method to all other existing subclasses.



"
Class {
	#name : 'RBPullUpMethodRefactoring',
	#superclass : 'RBMethodRefactoring',
	#instVars : [
		'removeDuplicates',
		'selectors',
		'targetSuperclass'
	],
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'instance creation' }
RBPullUpMethodRefactoring class >> model: aRBSmalltalk pullUp: selectorCollection from: aClass [
	^ self new
		model: aRBSmalltalk;
		pullUp: selectorCollection from: aClass;
		yourself
]

{ #category : 'instance creation' }
RBPullUpMethodRefactoring class >> model: aRBSmalltalk pullUp: selectorCollection from: aClass to: aSuperClass [
	^ self new
		model: aRBSmalltalk;
		pullUp: selectorCollection from: aClass to: aSuperClass;
		yourself
]

{ #category : 'instance creation' }
RBPullUpMethodRefactoring class >> pullUp: selectorCollection from: aClass [
	^ self new
		pullUp: selectorCollection from: aClass
]

{ #category : 'instance creation' }
RBPullUpMethodRefactoring class >> pullUp: selectorCollection from: aClass to: aSuperClass [
	^ self new
		pullUp: selectorCollection from: aClass to: aSuperClass
]

{ #category : 'preconditions' }
RBPullUpMethodRefactoring >> applicabilityPreconditions [

	^ {
		  (RBCondition hasSuperclass: class).
		  (ReDefinesSelectorsCondition new
			   definesSelectors: selectors
			   in: class).
		  (RBCondition withBlock: [
			   self checkClassVars.
			   self checkSuperclass.
			   self checkSuperMessages.
			   true ]) }
]

{ #category : 'preconditions' }
RBPullUpMethodRefactoring >> breakingChangePreconditions [

	^ { (RBCondition withBlock: [
		   self checkInstVars.
		   true ]) }
]

{ #category : 'preconditions' }
RBPullUpMethodRefactoring >> checkBackReferencesTo: aSelector [
	"If `aSelector` is defined in `targetSuperclass` or one of its superclasses check for
	super sends to `aSelector` in `targetSuperclass` hierarchy.
	Display an error to the user about potential breaking changes with overriding `aSelector`
	in `targetSuperclass` "

	| definingClass pushUpParseTree |
	definingClass := targetSuperclass whichClassIncludesSelector:
		                 aSelector.
	definingClass ifNil: [ ^ self ].
	pushUpParseTree := class parseTreeForSelector: aSelector.
	targetSuperclass allSubclasses do: [ :each |
		each selectors do: [ :sel |
			| parseTree |
			parseTree := each parseTreeForSelector: sel.
			(parseTree isNotNil and: [
				 (parseTree superMessages includes: aSelector) and: [
					 definingClass == (each whoDefinesMethod: aSelector) ] ])
				ifTrue: [
					removeDuplicates := true.
					(aSelector == sel and: [
						 parseTree equalTo: pushUpParseTree exceptForVariables: #(  ) ])
						ifFalse: [
							self refactoringError:
								('Cannot pull up <1s> since it would override the method defined in <2p>'
									 expandMacrosWith: aSelector
									 with: definingClass) ] ] ] ]
]

{ #category : 'preconditions' }
RBPullUpMethodRefactoring >> checkClassVars [
	selectors do: [:each | self checkClassVarsFor: each]
]

{ #category : 'preconditions' }
RBPullUpMethodRefactoring >> checkClassVarsFor: aSelector [
	"Checks if `aSelector` refers to any of the class variables."

	class instanceSide classVariableNames do:
		[ :each |
		((class whichSelectorsReferToClassVariable: each) includes: aSelector) ifTrue:
			[ self refactoringError: ('<1p> refers to <2s> which is defined in <3p>'
					expandMacrosWith: aSelector
					with: each
					with: class) ] ]
]

{ #category : 'preconditions' }
RBPullUpMethodRefactoring >> checkInstVars [
	selectors do: [:each | self checkInstVarsFor: each]
]

{ #category : 'preconditions' }
RBPullUpMethodRefactoring >> checkInstVarsFor: aSelector [
	"Checks if `aSelector` refers to one of the instance variables and offers to perform
	`PushUpInstanceVariableRefactoring` refactoring such variables exist."

	class instanceVariableNames do:
			[:each |
			((class whichSelectorsReferToInstanceVariable: each) includes: aSelector) ifTrue:
					[ (self confirm: ('<1p> refers to #<2s> which is defined in <3p>. Do you want push up variable #<2s> also?' expandMacrosWith: aSelector
								with: each
								with: class))
						ifTrue: [ self pushUpVariable: each ]
						ifFalse: [ self refactoringWarning: 'You are about to push your method without the instance variable it uses.
						It will bring the system is an inconsistent state. But this may be what you want.
						So do you want to push up anyway?' ] ]]
]

{ #category : 'preconditions' }
RBPullUpMethodRefactoring >> checkSiblingSuperSendsFrom: aRBClass [
	"Checks if `aRBClass` selector sends super message to one of the methods selected to be push up
	from the `class`.
	If that stands and the super message selector is defined in `class` or one of its superclasses
	warn the user.
	Repeat the process for all subclasses of `aRBClass`"

	aRBClass selectors do: [ :each |
		| tree |
		tree := aRBClass parseTreeForSelector: each.
		tree ifNotNil: [
			tree superMessages do: [ :aSelector |
				(selectors includes: aSelector) ifTrue: [
					| definer |
					definer := aRBClass superclass whichClassIncludesSelector:
						           aSelector.
					(definer isNotNil and: [ class includesClass: definer ]) ifTrue: [
						self refactoringError:
							('Cannot pull up <1s> since <2p>>><3s> sends a super message to it.'
								 expandMacrosWith: aSelector
								 with: aRBClass
								 with: each) ] ] ] ] ].
	aRBClass allSubclasses do: [ :each |
		self checkSiblingSuperSendsFrom: each ]
]

{ #category : 'preconditions' }
RBPullUpMethodRefactoring >> checkSuperMessages [
	self checkSuperSendsFromPushedUpMethods.
	self checkSuperSendsFromSiblings
]

{ #category : 'preconditions' }
RBPullUpMethodRefactoring >> checkSuperSendsFromPushedUpMethods [
	"Check if one of the selectors sends a super message to a method defined in `targetSuperclass`."

	selectors do:
			[:each |
			| parseTree |
			parseTree := class parseTreeForSelector: each.
			parseTree superMessages
				detect: [:sup | targetSuperclass directlyDefinesMethod: sup]
				ifFound:
					[self
						refactoringError: ('Cannot pull up <1s> since it sends a super message that is defined in the superclass.'
								expandMacrosWith: each)]]
]

{ #category : 'preconditions' }
RBPullUpMethodRefactoring >> checkSuperSendsFromSiblings [
	"Checks if a class in the `targetClass` subclasses (except for `class` and its subclasses)
	send super message to one of the selected messages."

	| siblings |
	siblings := targetSuperclass subclasses reject: [:each | each = class].
	siblings do: [:aRBClass | self checkSiblingSuperSendsFrom: aRBClass]
]

{ #category : 'preconditions' }
RBPullUpMethodRefactoring >> checkSuperclass [
	"Checks if one of the methods to push up is overriden from the `targetSuperclass`.
	If there are overrides and `targetSuperclass` is not abstract a pop-up warns the user.
	Also, if there are overrides check for back references (that is if one of the other classes in
	`targetSuperclass` hierarchy depends on original method defined in `targetSuperclass`)."

	| overrideSelectors |
	overrideSelectors := selectors
		select: [ :each | targetSuperclass directlyDefinesMethod: each ].
	overrideSelectors := overrideSelectors
		reject: [ :each |
			| myTree superTree |
			myTree := class parseTreeForSelector: each.
			superTree := targetSuperclass parseTreeForSelector: each.
			superTree equalTo: myTree exceptForVariables: #() ].
	overrideSelectors ifEmpty: [ ^ self ].
	targetSuperclass isAbstract
		ifFalse: [ self
				refactoringError:
					('Non-abstract class <2p> already defines <1p>'
						expandMacrosWith: overrideSelectors asArray first
						with: targetSuperclass) ].
	overrideSelectors do: [ :each | self checkBackReferencesTo: each ]
]

{ #category : 'private' }
RBPullUpMethodRefactoring >> copyDownMethod: aSelector [

	| oldProtocol oldSource superclassDefiner subclasses refactoring |
	superclassDefiner := targetSuperclass whichClassIncludesSelector:
		                     aSelector.
	superclassDefiner ifNil: [ ^ self ].
	oldSource := superclassDefiner sourceCodeFor: aSelector.
	oldSource ifNil: [
		self refactoringError:
			('Source code for <1s> superclass method not available'
				 expandMacrosWith: aSelector) ].
	oldProtocol := superclassDefiner protocolsFor: aSelector.
	subclasses := targetSuperclass subclasses reject: [ :each |
		              each directlyDefinesMethod: aSelector ].
	subclasses ifEmpty: [ ^ self ].
	(superclassDefiner parseTreeForSelector: aSelector) superMessages
		detect: [ :each | superclassDefiner directlyDefinesMethod: each ]
		ifFound: [
			self refactoringError:
				('Cannot pull up <1s> since we must copy down the superclass method in <2p><n>to the other subclasses, and the superclass method sends a super message which is overriden.'
					 expandMacrosWith: aSelector
					 with: superclassDefiner) ].
	self refactoringWarning:
		'Do you want to copy down the superclass method to the classes that don''t define '
		, aSelector , '?'.
	refactoring := RBExpandReferencedPoolsRefactoring
		               model: self model
		               forMethod:
		               (superclassDefiner parseTreeForSelector: aSelector)
		               fromClass: superclassDefiner
		               toClasses: subclasses.
	self generateChangesFor: refactoring.
	subclasses do: [ :each |
		self generateChangesFor: (RBAddMethodTransformation
				 sourceCode: oldSource
				 in: each
				 withProtocol: oldProtocol) ]
]

{ #category : 'transforming' }
RBPullUpMethodRefactoring >> copyDownMethods [
	selectors do: [:each | self copyDownMethod: each]
]

{ #category : 'preconditions' }
RBPullUpMethodRefactoring >> preconditions [

	^ self applicabilityPreconditions & self breakingChangePreconditions 
]

{ #category : 'transforming' }
RBPullUpMethodRefactoring >> privateTransform [
	self
		copyDownMethods;
		pullUpMethods;
		removePulledUpMethods;
		removeDuplicateMethods
]

{ #category : 'transforming' }
RBPullUpMethodRefactoring >> pullUp: aSelector [
	| source refactoring |
	source := class sourceCodeFor: aSelector.
	source ifNil: [self refactoringError: 'Source for method not available'].
	refactoring := RBExpandReferencedPoolsRefactoring
				model: self model
				forMethod: (class parseTreeForSelector: aSelector)
				fromClass: class
				toClasses: (Array with: targetSuperclass).
	self generateChangesFor: refactoring.
	self generateChangesFor:
		(RBAddMethodTransformation
			sourceCode: source
			in: targetSuperclass 
			withProtocol: (class protocolsFor: aSelector))
]

{ #category : 'initialization' }
RBPullUpMethodRefactoring >> pullUp: selectorCollection from: aClass [
	self pullUp: selectorCollection from: aClass to: aClass superclass
]

{ #category : 'initialization' }
RBPullUpMethodRefactoring >> pullUp: selectorCollection from: aClass to: aSuperClass [

	class := self classObjectFor: aClass.
	targetSuperclass := self classObjectFor: aSuperClass .
	selectors := selectorCollection.
	removeDuplicates := false
]

{ #category : 'transforming' }
RBPullUpMethodRefactoring >> pullUpMethods [
	selectors do: [:each | self pullUp: each]
]

{ #category : 'preconditions' }
RBPullUpMethodRefactoring >> pushUpVariable: aVariable [
	| refactoring |
	refactoring :=  RBPullUpInstanceVariableRefactoring
			model: self model
			variable: aVariable
			class: targetSuperclass.
	self generateChangesFor: refactoring
]

{ #category : 'transforming' }
RBPullUpMethodRefactoring >> removeDuplicateMethods [
	selectors do: [:each | self removeDuplicatesOf: each]
]

{ #category : 'transforming' }
RBPullUpMethodRefactoring >> removeDuplicatesOf: aSelector [

	| tree |
	tree := targetSuperclass parseTreeForSelector: aSelector.
	targetSuperclass allSubclasses do: [ :each |
		((each directlyDefinesMethod: aSelector) and: [
			 (tree
				  equalTo: (each parseTreeForSelector: aSelector)
				  exceptForVariables: #(  )) and: [
				 (each superclass whichClassIncludesSelector: aSelector)
				 == targetSuperclass ] ]) ifTrue: [
			removeDuplicates ifFalse: [
				removeDuplicates := self refactoringConfirmWarning:
					                    'Do you want to remove duplicate subclass methods?' ].
			removeDuplicates ifTrue: [
				self generateChangesFor:
					(RBRemoveMethodTransformation selector: aSelector from: each) ] ] ]
]

{ #category : 'transforming' }
RBPullUpMethodRefactoring >> removePulledUpMethods [

	selectors do: [ :each |
		self generateChangesFor:
			(RBRemoveMethodTransformation selector: each from: class) ]
]

{ #category : 'initialization' }
RBPullUpMethodRefactoring >> selectors [
	^ selectors
]

{ #category : 'initialization' }
RBPullUpMethodRefactoring >> selectors: aList [
	selectors := aList
]

{ #category : 'storing' }
RBPullUpMethodRefactoring >> storeOn: aStream [
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' pullUp: '.
	selectors asArray storeOn: aStream.
	aStream nextPutAll: ' from: '.
	class storeOn: aStream.
	aStream nextPut: $)
]

{ #category : 'initialization' }
RBPullUpMethodRefactoring >> superClass: anObject [
	(anObject allSubclasses includes: class realClass )
		ifFalse: [ self refactoringError:
			('<1s> is not a superclass of <2p>' expandMacrosWith: anObject with: class name ) ].
	targetSuperclass := self classObjectFor: anObject
]

{ #category : 'initialization' }
RBPullUpMethodRefactoring >> targetClass [
	^ class
]
