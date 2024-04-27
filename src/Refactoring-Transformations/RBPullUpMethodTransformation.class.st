Class {
	#name : 'RBPullUpMethodTransformation',
	#superclass : 'RBCompositeMethodTransformation',
	#instVars : [
		'removeDuplicates',
		'selectors',
		'targetSuperclass'
	],
	#category : 'Refactoring-Transformations-Model-Migrated-Unused',
	#package : 'Refactoring-Transformations',
	#tag : 'Model-Migrated-Unused'
}

{ #category : 'api' }
RBPullUpMethodTransformation class >> model: aRBNamespace pullUp: selectorCollection from: aClass [
	^ self new
		model: aRBNamespace;
		pullUp: selectorCollection from: aClass;
		yourself
]

{ #category : 'api' }
RBPullUpMethodTransformation class >> model: aRBNamespace pullUp: selectorCollection from: aClass to: aSuperClass [
	^ self new
		model: aRBNamespace;
		pullUp: selectorCollection from: aClass to: aSuperClass;
		yourself
]

{ #category : 'api' }
RBPullUpMethodTransformation class >> pullUp: selectorCollection from: aClass [
	^ self new pullUp: selectorCollection from: aClass
]

{ #category : 'api' }
RBPullUpMethodTransformation class >> pullUp: selectorCollection from: aClass to: aSuperClass [
	^ self new pullUp: selectorCollection from: aClass to: aSuperClass
]

{ #category : 'preconditions' }
RBPullUpMethodTransformation >> applicabilityPreconditions [ 

	^ (selectors 
			inject: (RBCondition hasSuperclass: class)
			into: [:cond :each | cond & (RBCondition definesSelector: each in: class)])
					& (RBCondition withBlock:
							[self checkInstVars.
							self checkClassVars.
							self checkSuperclass.
							self checkSuperMessages.
							true])
]

{ #category : 'executing' }
RBPullUpMethodTransformation >> buildTransformations [
	^ OrderedCollection new
		addAll: self copyDownMethods;
		addAll: self pullUpMethods;
		addAll: self removePulledUpMethods;
		addAll: self removeDuplicateMethods;
		removeAllSuchThat: [ :e | e isNil ];
		yourself
]

{ #category : 'preconditions' }
RBPullUpMethodTransformation >> checkBackReferencesTo: aSelector [

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
RBPullUpMethodTransformation >> checkClassVars [
	selectors do: [:each | self checkClassVarsFor: each]
]

{ #category : 'preconditions' }
RBPullUpMethodTransformation >> checkClassVarsFor: aSelector [
	class instanceSide classVariableNames do:
		[ :each |
		((class whichSelectorsReferToClassVariable: each) includes: aSelector) ifTrue:
			[ self refactoringError: ('<1p> refers to <2s> which is defined in <3p>'
					expandMacrosWith: aSelector
					with: each
					with: class) ] ]
]

{ #category : 'preconditions' }
RBPullUpMethodTransformation >> checkInstVars [
	selectors do: [:each | self checkInstVarsFor: each]
]

{ #category : 'preconditions' }
RBPullUpMethodTransformation >> checkInstVarsFor: aSelector [
	class instanceVariableNames do:
			[:each |
			((class whichSelectorsReferToInstanceVariable: each) includes: aSelector) ifTrue:
					[ (self confirm: ('<1p> refers to #<2s> which is defined in <3p>. Do you want push up variable #<2s> also?' expandMacrosWith: aSelector
								with: each
								with: class))
						ifTrue: [ self previousTransformations add: (self pushUpVariable: each) ]
						ifFalse: [ self refactoringWarning: 'You are about to push your method without the instance variable it uses.
						It will bring the system is an inconsistent state. But this may be what you want.
						So do you want to push up anyway?' ] ]]
]

{ #category : 'preconditions' }
RBPullUpMethodTransformation >> checkSiblingSuperSendsFrom: aRBClass [

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
RBPullUpMethodTransformation >> checkSuperMessages [
	self checkSuperSendsFromPushedUpMethods.
	self checkSuperSendsFromSiblings
]

{ #category : 'preconditions' }
RBPullUpMethodTransformation >> checkSuperSendsFromPushedUpMethods [
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
RBPullUpMethodTransformation >> checkSuperSendsFromSiblings [
	| siblings |
	siblings := targetSuperclass subclasses reject: [:each | each = class].
	siblings do: [:aRBClass | self checkSiblingSuperSendsFrom: aRBClass]
]

{ #category : 'preconditions' }
RBPullUpMethodTransformation >> checkSuperclass [
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

{ #category : 'executing' }
RBPullUpMethodTransformation >> copyDownMethod: aSelector [

	| oldProtocol oldSource superclassDefiner subclasses refactoring |
	superclassDefiner := targetSuperclass whichClassIncludesSelector:
		                     aSelector.
	superclassDefiner ifNil: [ ^ {  } ].
	oldSource := superclassDefiner sourceCodeFor: aSelector.
	oldSource ifNil: [
		self refactoringError:
			('Source code for <1s> superclass method not available'
				 expandMacrosWith: aSelector) ].
	oldProtocol := superclassDefiner protocolsFor: aSelector.
	subclasses := targetSuperclass subclasses reject: [ :each |
		              each directlyDefinesMethod: aSelector ].
	subclasses ifEmpty: [ ^ {  } ].
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
	refactoring := RBExpandReferencedPoolsTransformation
		               model: self model
		               forMethod:
		               (superclassDefiner parseTreeForSelector: aSelector)
		               fromClass: superclassDefiner
		               toClasses: subclasses.
	^ Array with: refactoring withAll: (subclasses collect: [ :each |
			   RBAddMethodTransformation
				   model: self model
				   sourceCode: oldSource
				   in: each
				   withProtocol: oldProtocol ])
]

{ #category : 'executing' }
RBPullUpMethodTransformation >> copyDownMethods [
	^ selectors flatCollect: [:each | self copyDownMethod: each]
]

{ #category : 'executing' }
RBPullUpMethodTransformation >> pullUp: aSelector [
	| source refactoring |
	source := class sourceCodeFor: aSelector.
	source ifNil: [self refactoringError: 'Source for method not available'].
	refactoring := RBExpandReferencedPoolsTransformation
				model: self model
				forMethod: (class parseTreeForSelector: aSelector)
				fromClass: class
				toClasses: (Array with: targetSuperclass).
	^ Array
		with: refactoring
		with: (RBAddMethodTransformation
		model: self model
		sourceCode: source
		in: targetSuperclass
		withProtocol: (class protocolsFor: aSelector))
]

{ #category : 'api' }
RBPullUpMethodTransformation >> pullUp: selectorCollection from: aClass [
	self pullUp: selectorCollection from: aClass to: aClass superclass
]

{ #category : 'api' }
RBPullUpMethodTransformation >> pullUp: selectorCollection from: aClass to: aSuperClass [

	class := self model classObjectFor: aClass.
	targetSuperclass := self model classObjectFor: aSuperClass.
	selectors := selectorCollection.
	removeDuplicates := false
]

{ #category : 'executing' }
RBPullUpMethodTransformation >> pullUpMethods [
	^ selectors flatCollect: [:each | self pullUp: each]
]

{ #category : 'preconditions' }
RBPullUpMethodTransformation >> pushUpVariable: aVariable [
	^ RBPullUpVariableTransformation
			model: self model
			instanceVariable: aVariable
			class: targetSuperclass
]

{ #category : 'executing' }
RBPullUpMethodTransformation >> removeDuplicateMethods [
	^ selectors collect: [:each | self removeDuplicatesOf: each]
]

{ #category : 'executing' }
RBPullUpMethodTransformation >> removeDuplicatesOf: aSelector [
	^ RBCustomTransformation
		model: self model
		with: [ :rbModel |
			| tree |
			tree := targetSuperclass parseTreeForSelector: aSelector.
			targetSuperclass allSubclasses collect:
			[:aClass |
			((aClass directlyDefinesMethod: aSelector) and:
					[(tree equalTo: (aClass parseTreeForSelector: aSelector) exceptForVariables: #())
						and: [(aClass superclass whoDefinesMethod: aSelector) == targetSuperclass]])
				ifTrue:
					[removeDuplicates
						ifFalse:
							[removeDuplicates := self refactoringConfirmWarning:
							'Do you want to remove duplicate subclass methods?'].
					removeDuplicates ifTrue:[
						(RBRemoveMethodTransformation
							model: self model
							selector: aClass
							from: aSelector) generateChanges ]]]]
]

{ #category : 'executing' }
RBPullUpMethodTransformation >> removePulledUpMethods [
	^ selectors collect: [:symbol |
		RBRemoveMethodTransformation
			model: self model
			selector: symbol
			from: class ]
]

{ #category : 'storing' }
RBPullUpMethodTransformation >> storeOn: aStream [
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream
		nextPutAll: ' pullUp: ';
		nextPutAll: selectors asString;
		nextPutAll: ' from: ';
		nextPutAll: class name;
		nextPutAll: ' to: ';
		nextPutAll: targetSuperclass name.
	aStream nextPut: $)
]
