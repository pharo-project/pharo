Class {
	#name : #RBNamespaceTest,
	#superclass : #RBRefactoringBrowserTest,
	#instVars : [
		'modelFactory'
	],
	#category : #'Refactoring-Tests-Core'
}

{ #category : #running }
RBNamespaceTest >> setUp [ 
	super setUp.
	modelFactory := RBClassModelFactory new. 
]

{ #category : #'class tests' }
RBNamespaceTest >> testAllClassesDo [
	| model classes |
	classes := 0.
	model := modelFactory rbNamespace new.
	model allClassesDo: 
			[:each | 
			each name = #Object ifTrue: [each allSubclasses].
			classes := classes + 1].
	RBBrowserEnvironment new classesDo: [:each | classes := classes - 1].
	self assert: classes equals: 0
]

{ #category : #'class tests' }
RBNamespaceTest >> testCommentChange [
	| st cl |
	st := modelFactory rbNamespace new.
	cl := st classNamed: self class name.
	self assert: cl comment isString.
	cl comment: 'a comment'.
	self assert: cl comment equals: 'a comment'.
	self assert: st changes changes size equals: 1.
	self assert: st changes changes first comment equals: 'a comment'.
	cl comment: nil.
	self assert: cl comment isNil.
	self assert: st changes changes size equals: 2.
	self assert: st changes changes last comment isNil
]

{ #category : #'class tests' }
RBNamespaceTest >> testDefineClassAfterDeletedChange [
	| st |
	st := modelFactory rbNamespace new.
	st removeClassNamed: self class name.
	self deny: (st includesClassNamed: self class name).
	st defineClass: self class oldDefinition.
	self assert: (st includesClassNamed: self class name).
	self assert: (st classNamed: self class name) notNil
]

{ #category : #'class tests' }
RBNamespaceTest >> testDefineClassChange [
	| st |
	st := modelFactory rbNamespace new.
	st 
		defineClass: 'RefactoringBrowserTest subclass: #SmalltalkTestXXX
				instanceVariableNames: ''''
				classVariableNames: ''''
				poolDictionaries: ''''
				category: #''Refactory-Testing'''.
	self assert: (st includesClassNamed: #SmalltalkTestXXX).
	self assert: (st classNamed: #SmalltalkTestXXX) notNil
]

{ #category : #'class tests' }
RBNamespaceTest >> testImplementors [
	| st |
	st := modelFactory rbNamespace new.
	self assert: ((st allImplementorsOf: #printString) 
				includes: (st classNamed: #Object)).
	(st classNamed: #Object) removeMethod: #printString.
	self deny: ((st allImplementorsOf: #printString) 
				includes: (st classNamed: #Object))
]

{ #category : #'class tests' }
RBNamespaceTest >> testIncludesClass [
	self assert: (modelFactory rbNamespace new includesClassNamed: #Object).
	self deny: (modelFactory rbNamespace new includesClassNamed: #Object1).
	self 
		deny: ((modelFactory rbNamespace 
				onEnvironment: (RBClassEnvironment onEnvironment: RBBrowserEnvironment new
						classes: (Array with: Object))) 
					includesClassNamed: #OrderedCollection)
]

{ #category : #'class tests' }
RBNamespaceTest >> testModelImplementorsSenders [
	| model class modelImps refs found |
	model := modelFactory rbNamespace new.
	model 
		defineClass: 'Object subclass: #Asdf
			instanceVariableNames: ''''
			classVariableNames: ''''
			poolDictionaries: ''''
			category: #''Refactory-Testing'''.
	class := model classNamed: #Asdf.
	class compile: 'storeOn: aStream ^super storeOn: aStream'
		classified: #(#printing).
	modelImps := model allImplementorsOf: #storeOn:.
	self assert: (modelImps includes: class).
	self assert: modelImps size - 1 
				equals: (RBBrowserEnvironment new implementorsOf: #storeOn:) numberSelectors.
	refs := RBBrowserEnvironment new referencesTo: #storeOn:.
	found := false.
	model allReferencesTo: #storeOn:
		do: 
			[:each | 
			each modelClass = class 
				ifTrue: [found := true]
				ifFalse: 
					[	self 
						assert: (refs includesSelector: each selector in: each modelClass realClass)]].
	self assert: found

]

{ #category : #'class tests' }
RBNamespaceTest >> testRedefineClassChange [
	| st |
	st := modelFactory rbNamespace new.
	st defineClass: 'nil subclass: #Object
				instanceVariableNames: ''a''
				classVariableNames: ''A''
				poolDictionaries: ''TextConstants''
				category: #''Refactory-Testing'''.
	self assert: (st includesClassNamed: #Object).
	self assert: (st classNamed: #Object) notNil
]

{ #category : #'class tests' }
RBNamespaceTest >> testReferencesPrintOn [
	| hasFoundObject hasFoundSelf smalltalk |
	hasFoundObject := false.
	hasFoundSelf := false.
	smalltalk := modelFactory rbNamespace new.
	smalltalk allReferencesTo: #printOn: do: [ :each | 
		hasFoundObject := hasFoundObject
			or: [ each selector = #fullPrintString 
			and: [each modelClass = (smalltalk classNamed: #Object) ] ].
		hasFoundSelf := hasFoundSelf
			or: [ each selector = #testReferencesPrintOn 
			and: [ each modelClass = (smalltalk classNamed: self class name) ] ] ].
	self assert: hasFoundObject.
	self assert: hasFoundSelf
]

{ #category : #'class tests' }
RBNamespaceTest >> testReferencesPrintOnAfterAddition [
	| hasFoundObject hasFoundSelf smalltalk |
	hasFoundObject := false.
	hasFoundSelf := false.
	smalltalk := modelFactory rbNamespace new.
	(smalltalk classNamed: #Object) 
		compile: 'someTestReference self printOn: nil'
		classified: #(testing).
	(smalltalk classNamed: self class name)
		compile: 'someTestReference ^ #printOn:'
		classified: #(testing).
	smalltalk allReferencesTo: #printOn: do: [ :each | 
		hasFoundObject := hasFoundObject 
			or: [ each selector = #someTestReference 
			and: [ each modelClass = (smalltalk classNamed: #Object) ] ].
		hasFoundSelf := hasFoundSelf
			or: [ each selector = #someTestReference 
			and: [ each modelClass = (smalltalk classNamed: self class name) ] ] ].
	self assert: hasFoundObject.
	self assert: hasFoundSelf
]

{ #category : #'class tests' }
RBNamespaceTest >> testReferencesPrintOnAfterRemove [
	| hasFoundObject hasFoundSelf smalltalk |
	hasFoundObject := false.
	hasFoundSelf := false.
	smalltalk := modelFactory rbNamespace new.
	(smalltalk classNamed: #Object) 
		removeMethod: #printString.
	(smalltalk classNamed: self class name) 
		removeMethod: #testReferencesPrintOnAfterRemove.
	smalltalk allReferencesTo: #printOn: do: [ :each | 
		hasFoundObject := hasFoundObject
			or: [ each selector = #printString 
			and: [ each modelClass = (smalltalk classNamed: #Object) ] ].
		hasFoundSelf := hasFoundSelf
			or: [ each selector = #testReferencesPrintOnAfterRemove 
			and: [ each modelClass = (smalltalk classNamed: self class name) ] ] ].
	self deny: hasFoundObject.
	self deny: hasFoundSelf
]

{ #category : #'class tests' }
RBNamespaceTest >> testRemoveClassChange [
	| st |
	st := modelFactory rbNamespace new.
	st removeClassNamed: self class name.
	self deny: (st includesClassNamed: self class name).
	self assert: (st classNamed: self class name) isNil
]

{ #category : #'class tests' }
RBNamespaceTest >> testReparentSuperclassChange [
	| st superclass subclasses |
	st := modelFactory rbNamespace new.
	superclass := st classFor: TestCase superclass.
	subclasses := TestCase subclasses collect: [:each | st classFor: each].
	st reparentClasses: subclasses to: superclass.
	subclasses do: [:each | self assert: each superclass equals: superclass]
]

{ #category : #'class tests' }
RBNamespaceTest >> testRoots [
	| model |
	model := modelFactory rbNamespace new.
	self 
		assert: (model rootClasses asSortedCollection: [:a :b | a name < b name]) 
				asArray 
		equals: ((Class rootsOfTheWorld collect: [:each | model classFor: each]) 
							asSortedCollection: [:a :b | a name < b name]) asArray
]
