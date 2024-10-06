"
I am a refactoring for moving a method up to the superclass. 

My precondition verify that this method does not refere to instance variables not accessible in the superclass. And this method does not sends a super message that is defined in the superclass.
If the method already exists and the superclass is abstract or not referenced anywhere, replace that implementation and push down the old method to all other existing subclasses.



"
Class {
	#name : 'RePullUpMethodRefactoring',
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
RePullUpMethodRefactoring class >> model: aRBSmalltalk pullUp: selectorCollection from: aClass [
	^ self new
		model: aRBSmalltalk;
		pullUp: selectorCollection from: aClass;
		yourself
]

{ #category : 'instance creation' }
RePullUpMethodRefactoring class >> model: aRBSmalltalk pullUp: selectorCollection from: aClass to: aSuperClass [
	^ self new
		model: aRBSmalltalk;
		pullUp: selectorCollection from: aClass to: aSuperClass;
		yourself
]

{ #category : 'instance creation' }
RePullUpMethodRefactoring class >> pullUp: selectorCollection from: aClass [
	^ self new
		pullUp: selectorCollection from: aClass
]

{ #category : 'instance creation' }
RePullUpMethodRefactoring class >> pullUp: selectorCollection from: aClass to: aSuperClass [
	^ self new
		pullUp: selectorCollection from: aClass to: aSuperClass
]

{ #category : 'preconditions' }
RePullUpMethodRefactoring >> applicabilityPreconditions [
	" Check that all selectors are defined in `class`, 
	and that `class` is in the hierarchy of subclasses of `targetSuperclass` "

	^ {
		  (ReDefinesSelectorsCondition new
			   definesSelectors: selectors
			   in: class).
		  (ReClassHasSubclassesCondition new
			   class: targetSuperclass;
			   subclassesList: { class name }).
		  self preconditionNoReferencesToSharedVars.
		  self preconditionNoOverrides.
		  self preconditionNoSupersendsSent.
		  self preconditionNoSupersendsReceived }
]

{ #category : 'preconditions' }
RePullUpMethodRefactoring >> breakingChangePreconditions [

	^ { self preconditionNoReferencesToInstVars }
]

{ #category : 'preconditions' }
RePullUpMethodRefactoring >> checkBackReferencesTo: aSelector [
	"If `aSelector` is defined in `targetSuperclass` or one of its superclasses check for
	super sends to `aSelector` in `targetSuperclass` hierarchy.
	Display an error to the user about potential breaking changes with overriding `aSelector`
	in `targetSuperclass` "

	| definingClass pullUpParseTree |
	definingClass := targetSuperclass whichClassIncludesSelector:
		                 aSelector.
	definingClass ifNil: [ ^ self ].
	pullUpParseTree := class parseTreeForSelector: aSelector.
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
						 parseTree equalTo: pullUpParseTree exceptForVariables: #(  ) ])
						ifFalse: [
							self refactoringError:
								('Cannot pull up <1s> since it would override the method defined in <2p>'
									 expandMacrosWith: aSelector
									 with: definingClass) ] ] ] ]
]

{ #category : 'preconditions' }
RePullUpMethodRefactoring >> checkClassVars [
	selectors do: [:each | self checkClassVarsFor: each]
]

{ #category : 'preconditions' }
RePullUpMethodRefactoring >> checkClassVarsFor: aSelector [
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
RePullUpMethodRefactoring >> checkInstVars [
	selectors do: [:each | self checkInstVarsFor: each]
]

{ #category : 'preconditions' }
RePullUpMethodRefactoring >> checkInstVarsFor: aSelector [
	"Checks if `aSelector` refers to one of the instance variables and offers to perform
	`PullUpInstanceVariableRefactoring` refactoring such variables exist."

	class instanceVariableNames do:
			[:each |
			((class whichSelectorsReferToInstanceVariable: each) includes: aSelector) ifTrue:
					[ (self confirm: ('<1p> refers to #<2s> which is defined in <3p>. Do you want pull up variable #<2s> also?' expandMacrosWith: aSelector
								with: each
								with: class))
						ifTrue: [ self pullUpVariable: each ]
						ifFalse: [ self refactoringWarning: 'You are about to pull your method without the instance variable it uses.
						It will bring the system is an inconsistent state. But this may be what you want.
						So do you want to pull up anyway?' ] ]]
]

{ #category : 'preconditions' }
RePullUpMethodRefactoring >> checkSuperclass [
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
RePullUpMethodRefactoring >> copyDownMethod: aSelector [

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
RePullUpMethodRefactoring >> copyDownMethods [
	selectors do: [:each | self copyDownMethod: each]
]

{ #category : 'preconditions' }
RePullUpMethodRefactoring >> preconditionNoOverrides [

	^ (ReDefinesSelectorsCondition new
		   definesSelectors: selectors
		   in: targetSuperclass) not 
	  |
	  (ReClassesAreAbstractCondition new 
			classes: { targetSuperclass })
]

{ #category : 'preconditions' }
RePullUpMethodRefactoring >> preconditionNoReferencesToInstVars [

	^ ReMethodsDontReferToInstVarsCondition new
		  class: class
		  selectors: selectors
]

{ #category : 'preconditions' }
RePullUpMethodRefactoring >> preconditionNoReferencesToSharedVars [

	^ ReMethodsDontReferToSharedVarsCondition new
		   class: class
		   selectors: selectors
]

{ #category : 'preconditions' }
RePullUpMethodRefactoring >> preconditionNoSupersendsReceived [

	^ ReMethodsReceiveNoSupersendsCondition new
		   class: class
		   targetSuperclass: targetSuperclass
		   selectors: selectors
]

{ #category : 'preconditions' }
RePullUpMethodRefactoring >> preconditionNoSupersendsSent [

	^ ReMethodsSendNoSupersendsCondition new
		   class: class
		   targetSuperclass: targetSuperclass
		   selectors: selectors
]

{ #category : 'preconditions' }
RePullUpMethodRefactoring >> preconditions [

	^ self applicabilityPreconditions & self breakingChangePreconditions 
]

{ #category : 'transforming' }
RePullUpMethodRefactoring >> privateTransform [
	self
		copyDownMethods;
		pullUpMethods;
		removePulledUpMethods;
		removeDuplicateMethods
]

{ #category : 'transforming' }
RePullUpMethodRefactoring >> pullUp: aSelector [
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
RePullUpMethodRefactoring >> pullUp: selectorCollection from: aClass [
	self pullUp: selectorCollection from: aClass to: aClass superclass
]

{ #category : 'initialization' }
RePullUpMethodRefactoring >> pullUp: selectorCollection from: aClass to: aSuperClass [

	class := self classObjectFor: aClass.
	targetSuperclass := self classObjectFor: aSuperClass .
	selectors := selectorCollection.
	removeDuplicates := false
]

{ #category : 'transforming' }
RePullUpMethodRefactoring >> pullUpMethods [
	selectors do: [:each | self pullUp: each]
]

{ #category : 'transforming' }
RePullUpMethodRefactoring >> pullUpSharedVariable: aVariable [

	| refactoring |
	refactoring := RBPullUpClassVariableRefactoring
		               model: self model
		               variable: aVariable
		               class: targetSuperclass.
	self generateChangesFor: refactoring
]

{ #category : 'transforming' }
RePullUpMethodRefactoring >> pullUpVariable: aVariable [
	| refactoring |
	refactoring :=  RBPullUpInstanceVariableRefactoring
			model: self model
			variable: aVariable
			class: targetSuperclass.
	self generateChangesFor: refactoring
]

{ #category : 'cleaning after refactoring' }
RePullUpMethodRefactoring >> removeDuplicateMethods [
	
	"This behavior should only be used as an optimisation
	e.g. to clean up once the method is pull up and it only remove
	methods with the same name and body. Do not invoke this method in another    
	situation."
	
	selectors do: [ :each | self removeDuplicatesOf: each ]
]

{ #category : 'cleaning after refactoring' }
RePullUpMethodRefactoring >> removeDuplicatesOf: aSelector [

	| tree siblings |
	tree := class parseTreeForSelector: aSelector.
	siblings := targetSuperclass allSubclasses reject: [ :each | each = class ].
	
	siblings do: [ :each |
		((each directlyDefinesMethod: aSelector) and: [
			 (tree
				  equalTo: (each parseTreeForSelector: aSelector)
				  exceptForVariables: #(  )) ]) ifTrue: [
			self generateChangesFor:
				(RBRemoveMethodTransformation selector: aSelector from: each) ] ]. 
]

{ #category : 'transforming' }
RePullUpMethodRefactoring >> removePulledUpMethods [

	selectors do: [ :each |
		self generateChangesFor:
			(RBRemoveMethodTransformation selector: each from: class) ]
]

{ #category : 'initialization' }
RePullUpMethodRefactoring >> selectors [
	^ selectors
]

{ #category : 'initialization' }
RePullUpMethodRefactoring >> selectors: aList [
	selectors := aList
]

{ #category : 'storing' }
RePullUpMethodRefactoring >> storeOn: aStream [
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' pullUp: '.
	selectors asArray storeOn: aStream.
	aStream nextPutAll: ' from: '.
	class storeOn: aStream.
	aStream nextPut: $)
]

{ #category : 'initialization' }
RePullUpMethodRefactoring >> superClass: anObject [
	(anObject allSubclasses includes: class realClass )
		ifFalse: [ self refactoringError:
			('<1s> is not a superclass of <2p>' expandMacrosWith: anObject with: class name ) ].
	targetSuperclass := self classObjectFor: anObject
]

{ #category : 'initialization' }
RePullUpMethodRefactoring >> targetClass [
	^ class
]

{ #category : 'accessing' }
RePullUpMethodRefactoring >> targetSuperclass [

	^ targetSuperclass
]
