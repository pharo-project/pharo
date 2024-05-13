"
I am a refactoring operation for removing of usages of a deprecated class, that was renamed to another name.

 I'm doing following operations:
 - all subclasses of the deprecated class will use the new class as superclass (optional)
 - convert new class to superclass of deprecatedclass, remove methods of deprecated class and add class method #isDeprecated (optional)
 - rename all references in the code
 - move extensions of the deprecated class owned by other packages to the new class (optional)
 - remove the extensions (optional)

"
Class {
	#name : 'RBDeprecateClassRefactoring',
	#superclass : 'RBClassRefactoring',
	#instVars : [
		'newName',
		'deprecatedClass',
		'shouldCopyExtensions',
		'shouldRemoveExtensions',
		'shouldFixSubclasses'
	],
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'instance creation' }
RBDeprecateClassRefactoring class >> deprecate: aDeprecatedClass in: aNewName [
	^ self new className: aDeprecatedClass newName: aNewName
]

{ #category : 'instance creation' }
RBDeprecateClassRefactoring class >> model: aRBSmalltalk deprecate: aClass in: aNewName [
	^ self new
		model: aRBSmalltalk;
		className: aClass name newName: aNewName;
		yourself
]

{ #category : 'adding' }
RBDeprecateClassRefactoring >> addMethod: source to: aClass in: aProtocol [

	self generateChangesFor: 
		(RBAddMethodTransformation
			sourceCode: source
			in: aClass
			withProtocol: aProtocol)
]

{ #category : 'preconditions' }
RBDeprecateClassRefactoring >> applicabilityPreconditions [

	| class |
	class := self model classNamed: className.
	^ {
		  (ReClassesExistCondition new classes: { class }).
		  (ReClassesAreNotMetaClassCondition new classes: { class }).
		  (ReNameIsGlobalCondition new model: self model className: newName).
		  (ReValidClassNameCondition new className: newName) }
]

{ #category : 'scripting api - conditions' }
RBDeprecateClassRefactoring >> checkPreconditions [ 

	self checkApplicabilityPreconditions 
]

{ #category : 'initialization' }
RBDeprecateClassRefactoring >> className: aName newName: aNewName [
	className := aName asSymbol.
	deprecatedClass := self model classNamed: className.
	newName := aNewName asSymbol
]

{ #category : 'transforming' }
RBDeprecateClassRefactoring >> classSideExtensionMethodsOf: anRBClass [

	^ anRBClass realClass classSide localMethods
		select: [ :m | m isExtension ]
		thenCollect: [ :each | anRBClass classSide methodFor: each selector ]
]

{ #category : 'transforming' }
RBDeprecateClassRefactoring >> convertDeprecatedToSubclass [
	| replacement |
	replacement := self model classNamed: newName.
	self model reparentClasses: {deprecatedClass} to: replacement.
	deprecatedClass selectors do: [ :symbol |
		(deprecatedClass realClass instanceSide compiledMethodAt: symbol) isExtension
			ifFalse: [ self removeMethod: symbol from: deprecatedClass ] ].
	deprecatedClass classSide selectors do: [ :symbol |
		(deprecatedClass realClass classSide compiledMethodAt: symbol) isExtension
			ifFalse: [ self removeMethod: symbol from: deprecatedClass classSide ] ].
	self addMethod: 'isDeprecated
		" Uses ', newName, ' instead of ', className,'"
		^ true'
			to: deprecatedClass classSide
			in: 'testing'
]

{ #category : 'transforming' }
RBDeprecateClassRefactoring >> copyExtensionMethods [

	(self instanceSideExtensionMethodsOf: deprecatedClass)
		do: [ :each | self addMethod: each source to: self newClass instanceSide in: each protocols ].

	(self classSideExtensionMethodsOf: deprecatedClass)
		do: [ :each | self addMethod: each source to: self newClass classSide in: each protocols ]
]

{ #category : 'accessing' }
RBDeprecateClassRefactoring >> deprecatedClass [
	^ deprecatedClass
]

{ #category : 'accessing' }
RBDeprecateClassRefactoring >> deprecatedClass: anObject [
	deprecatedClass := anObject
]

{ #category : 'initialization' }
RBDeprecateClassRefactoring >> initialize [

	super initialize.

	shouldFixSubclasses := true.
	shouldCopyExtensions := true.
	shouldRemoveExtensions := true
]

{ #category : 'transforming' }
RBDeprecateClassRefactoring >> instanceSideExtensionMethodsOf: anRBClass [

	^ anRBClass realClass instanceSide localMethods
		select: #isExtension
		thenCollect: [ :each | anRBClass instanceSide methodFor: each selector ]
]

{ #category : 'transforming' }
RBDeprecateClassRefactoring >> newClass [

	^ self model classNamed: newName
]

{ #category : 'transforming' }
RBDeprecateClassRefactoring >> privateTransform [

	self shouldFixSubclasses
		ifTrue: [ self renameSuperclassOfSubclasses.
			self convertDeprecatedToSubclass ].

	self renameReferences.

	self shouldCopyExtensions
		ifTrue: [ self copyExtensionMethods ].

	self shouldRemoveExtensions
		ifTrue: [ self removeExtensionMethods ]
]

{ #category : 'transforming' }
RBDeprecateClassRefactoring >> removeExtensionMethods [

	(self instanceSideExtensionMethodsOf: deprecatedClass)
		do: [ :each | self removeMethod: each selector from: deprecatedClass instanceSide ].

	(self classSideExtensionMethodsOf: deprecatedClass)
		do: [ :each | self removeMethod: each selector from: deprecatedClass classSide ]
]

{ #category : 'removing' }
RBDeprecateClassRefactoring >> removeMethod: selector from: aRBClass [

	self generateChangesFor:
		(RBRemoveMethodTransformation
			selector: selector
			from: aRBClass)
]

{ #category : 'transforming' }
RBDeprecateClassRefactoring >> renameReferences [
	| replacer |
	replacer := (self parseTreeRewriterClass replaceLiteral: className with: newName)
		replace: className with: newName;
		replaceArgument: newName
		withValueFrom: [:aNode |
			self refactoringError: newName , ' already exists within the reference scope'];
		yourself.

	self model allReferencesToClass: deprecatedClass do: [ :method |
		(method modelClass hierarchyDefinesVariable: newName) ifTrue: [
			self refactoringError: newName , ' is already defined in hierarchy of ', method modelClass printString].

		self convertMethod: method selector
			for: method modelClass
			using: replacer ]
]

{ #category : 'transforming' }
RBDeprecateClassRefactoring >> renameSuperclassOfSubclasses [

	| replacement |
	replacement := self model classNamed: newName.
	self model reparentClasses: deprecatedClass subclasses to: replacement
]

{ #category : 'accessing' }
RBDeprecateClassRefactoring >> shouldCopyExtensions [
	^ shouldCopyExtensions
]

{ #category : 'accessing' }
RBDeprecateClassRefactoring >> shouldCopyExtensions: anObject [
	shouldCopyExtensions := anObject
]

{ #category : 'accessing' }
RBDeprecateClassRefactoring >> shouldFixSubclasses [
	^ shouldFixSubclasses
]

{ #category : 'accessing' }
RBDeprecateClassRefactoring >> shouldFixSubclasses: anObject [
	shouldFixSubclasses := anObject
]

{ #category : 'accessing' }
RBDeprecateClassRefactoring >> shouldRemoveExtensions [
	^ shouldRemoveExtensions
]

{ #category : 'accessing' }
RBDeprecateClassRefactoring >> shouldRemoveExtensions: anObject [
	shouldRemoveExtensions := anObject
]

{ #category : 'storing' }
RBDeprecateClassRefactoring >> storeOn: aStream [
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' rename: '.
	deprecatedClass storeOn: aStream.
	aStream
		nextPutAll: ' to: #';
		nextPutAll: newName;
		nextPut: $)
]
