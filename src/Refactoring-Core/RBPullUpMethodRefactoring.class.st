"
I am a refactoring for moving a method up to the superclass. 

My precondition verify that this method does not refere to instance variables not accessible in the superclass. And this method does not sends a super message that is defined in the superclass.
If the method already exists and the superclass is abstract or not referenced anywhere, replace that implementation and push down the old method to all other existing subclasses.



"
Class {
	#name : #RBPullUpMethodRefactoring,
	#superclass : #RBMethodRefactoring,
	#instVars : [
		'removeDuplicates',
		'selectors'
	],
	#category : #Refactoring-Core-Refactorings
}

{ #category : #'instance creation' }
RBPullUpMethodRefactoring class >> model: aRBSmalltalk pullUp: selectorCollection from: aClass [ 
	^(self new)
		model: aRBSmalltalk;
		pullUp: selectorCollection from: aClass;
		yourself
]

{ #category : #'instance creation' }
RBPullUpMethodRefactoring class >> pullUp: selectorCollection from: aClass [ 
	^self new pullUp: selectorCollection from: aClass
]

{ #category : #preconditions }
RBPullUpMethodRefactoring >> checkBackReferencesTo: aSelector [ 
	| definingClass pushUpParseTree |
	definingClass := class superclass whoDefinesMethod: aSelector.
	definingClass isNil ifTrue: [^self].
	pushUpParseTree := class parseTreeFor: aSelector.
	class superclass allSubclasses do: 
			[:each | 
			each selectors do: 
					[:sel | 
					| parseTree |
					parseTree := each parseTreeFor: sel.
					(parseTree notNil and: 
							[(parseTree superMessages includes: aSelector) 
								and: [definingClass == (each whoDefinesMethod: aSelector)]]) 
						ifTrue: 
							[removeDuplicates := true.
							(aSelector == sel 
								and: [parseTree equalTo: pushUpParseTree exceptForVariables: #()]) 
									ifFalse: 
										[self 
											refactoringError: ('Cannot pull up <1s> since it would override the method defined in <2p>' 
													expandMacrosWith: aSelector
													with: definingClass)]]]]
]

{ #category : #preconditions }
RBPullUpMethodRefactoring >> checkClassVars [
	selectors do: [:each | self checkClassVarsFor: each]
]

{ #category : #preconditions }
RBPullUpMethodRefactoring >> checkClassVarsFor: aSelector [ 
	class theNonMetaClass classVariableNames do: 
		[ :each | 
		((class whichSelectorsReferToClassVariable: each) includes: aSelector) ifTrue: 
			[ self refactoringError: ('<1p> refers to <2s> which is defined in <3p>' 
					expandMacrosWith: aSelector
					with: each
					with: class) ] ]
]

{ #category : #preconditions }
RBPullUpMethodRefactoring >> checkInstVars [
	selectors do: [:each | self checkInstVarsFor: each]
]

{ #category : #preconditions }
RBPullUpMethodRefactoring >> checkInstVarsFor: aSelector [ 
	class instanceVariableNames do: 
			[:each | 
			((class whichSelectorsReferToInstanceVariable: each) includes: aSelector) ifTrue: 
					[self
						refactoringError: ('<1p> refers to <2s> which is defined in <3p>' expandMacrosWith: aSelector
								with: each
								with: class)]]
]

{ #category : #preconditions }
RBPullUpMethodRefactoring >> checkSiblingSuperSendsFrom: aRBClass [ 
	aRBClass selectors do: 
			[:each | 
			| tree |
			tree := aRBClass parseTreeFor: each.
			tree notNil 
				ifTrue: 
					[tree superMessages do: 
							[:aSelector | 
							(selectors includes: aSelector) 
								ifTrue: 
									[| definer |
									definer := aRBClass superclass whoDefinesMethod: aSelector.
									(definer notNil and: [class includesClass: definer]) 
										ifTrue: 
											[self 
												refactoringError: ('Cannot pull up <1s> since <2p>>><3s> sends a super message to it.' 
														expandMacrosWith: aSelector
														with: aRBClass
														with: each)]]]]].
	aRBClass allSubclasses do: [:each | self checkSiblingSuperSendsFrom: each]
]

{ #category : #preconditions }
RBPullUpMethodRefactoring >> checkSuperMessages [
	self checkSuperSendsFromPushedUpMethods.
	self checkSuperSendsFromSiblings
]

{ #category : #preconditions }
RBPullUpMethodRefactoring >> checkSuperSendsFromPushedUpMethods [
	selectors do: 
			[:each | 
			| parseTree |
			parseTree := class parseTreeFor: each.
			(parseTree superMessages 
				detect: [:sup | class superclass directlyDefinesMethod: sup]
				ifNone: [nil]) notNil 
				ifTrue: 
					[self 
						refactoringError: ('Cannot pull up <1s> since it sends a super message that is defined in the superclass.' 
								expandMacrosWith: each)]]
]

{ #category : #preconditions }
RBPullUpMethodRefactoring >> checkSuperSendsFromSiblings [
	| siblings |
	siblings := class superclass subclasses reject: [:each | each = class].
	siblings do: [:aRBClass | self checkSiblingSuperSendsFrom: aRBClass]
]

{ #category : #preconditions }
RBPullUpMethodRefactoring >> checkSuperclass [
	| overrideSelectors |
	overrideSelectors := selectors 
				select: [:each | class superclass definesMethod: each].
	overrideSelectors := overrideSelectors reject: 
					[:each | 
					| myTree superTree |
					myTree := class parseTreeFor: each.
					superTree := class superclass parseTreeFor: each.
					superTree equalTo: myTree exceptForVariables: #()].
	overrideSelectors isEmpty ifTrue: [^self].
	class superclass isAbstract 
		ifFalse: 
			[self refactoringError: ('Non-abstract class <2p> already defines <1p>' 
						expandMacrosWith: overrideSelectors asArray first
						with: class superclass)].
	overrideSelectors do: [:each | self checkBackReferencesTo: each]
]

{ #category : #private }
RBPullUpMethodRefactoring >> copyDownMethod: aSelector [ 
	| oldProtocol oldSource superclassDefiner subclasses refactoring |
	superclassDefiner := class superclass whoDefinesMethod: aSelector.
	superclassDefiner isNil ifTrue: [^self].
	oldSource := superclassDefiner sourceCodeFor: aSelector.
	oldSource isNil 
		ifTrue: 
			[self 
				refactoringFailure: ('Source code for <1s> superclass method not available' 
						expandMacrosWith: aSelector)].
	oldProtocol := superclassDefiner protocolsFor: aSelector.
	subclasses := class superclass subclasses 
				reject: [:each | each directlyDefinesMethod: aSelector].
	subclasses isEmpty ifTrue: [^self].
	((superclassDefiner parseTreeFor: aSelector) superMessages 
		detect: [:each | superclassDefiner directlyDefinesMethod: each]
		ifNone: [nil]) notNil 
		ifTrue: 
			[self 
				refactoringError: ('Cannot pull up <1s> since we must copy down the superclass method in <2p><n>to the other subclasses, and the superclass method sends a super message which is overriden.' 
						expandMacrosWith: aSelector
						with: superclassDefiner)].
	self 
		refactoringWarning: 'Do you want to copy down the superclass method to the classes that don''t define ' 
				, aSelector.
	refactoring := RBExpandReferencedPoolsRefactoring 
				model: self model
				forMethod: (superclassDefiner parseTreeFor: aSelector)
				fromClass: superclassDefiner
				toClasses: subclasses.
	self performComponentRefactoring: refactoring.
	subclasses do: [:each | each compile: oldSource classified: oldProtocol]
]

{ #category : #transforming }
RBPullUpMethodRefactoring >> copyDownMethods [
	selectors do: [:each | self copyDownMethod: each]
]

{ #category : #preconditions }
RBPullUpMethodRefactoring >> preconditions [
	^(selectors inject: (RBCondition hasSuperclass: class)
		into: [:cond :each | cond & (RBCondition definesSelector: each in: class)]) 
			& (RBCondition withBlock: 
						[self checkInstVars.
						self checkClassVars.
						self checkSuperclass.
						self checkSuperMessages.
						true])
]

{ #category : #transforming }
RBPullUpMethodRefactoring >> pullUp: aSelector [ 
	| source refactoring |
	source := class sourceCodeFor: aSelector.
	source isNil 
		ifTrue: [self refactoringFailure: 'Source for method not available'].
	refactoring := RBExpandReferencedPoolsRefactoring 
				model: self model
				forMethod: (class parseTreeFor: aSelector)
				fromClass: class
				toClasses: (Array with: class superclass).
	self performComponentRefactoring: refactoring.
	class superclass compile: source
		classified: (class protocolsFor: aSelector)
]

{ #category : #initialize-release }
RBPullUpMethodRefactoring >> pullUp: selectorCollection from: aClass [ 
	class := self classObjectFor: aClass.
	selectors := selectorCollection.
	removeDuplicates := false
]

{ #category : #transforming }
RBPullUpMethodRefactoring >> pullUpMethods [
	selectors do: [:each | self pullUp: each]
]

{ #category : #transforming }
RBPullUpMethodRefactoring >> removeDuplicateMethods [
	selectors do: [:each | self removeDuplicatesOf: each]
]

{ #category : #transforming }
RBPullUpMethodRefactoring >> removeDuplicatesOf: aSelector [ 
	| tree |
	tree := class superclass parseTreeFor: aSelector.
	class superclass allSubclasses do: 
			[:each | 
			((each directlyDefinesMethod: aSelector) and: 
					[(tree equalTo: (each parseTreeFor: aSelector) exceptForVariables: #()) 
						and: [(each superclass whoDefinesMethod: aSelector) == class superclass]]) 
				ifTrue: 
					[removeDuplicates 
						ifFalse: 
							[removeDuplicates := 
							self 
								refactoringConfirmWarning: 'Do you want to remove duplicate subclass methods?'].
					removeDuplicates ifTrue:[
					each removeMethod: aSelector]]]
]

{ #category : #transforming }
RBPullUpMethodRefactoring >> removePulledUpMethods [
	selectors do: [:each | class removeMethod: each]
]

{ #category : #printing }
RBPullUpMethodRefactoring >> storeOn: aStream [ 
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' pullUp: '.
	selectors asArray storeOn: aStream.
	aStream nextPutAll: ' from: '.
	class storeOn: aStream.
	aStream nextPut: $)
]

{ #category : #transforming }
RBPullUpMethodRefactoring >> transform [
	self
		copyDownMethods;
		pullUpMethods;
		removePulledUpMethods;
		removeDuplicateMethods
]
