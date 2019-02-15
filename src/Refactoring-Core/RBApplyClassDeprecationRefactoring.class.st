"
I am a refactoring operation for removing of usages of a deprecated class, that was renamed to another name.

 I'm doing following operations:
 - all subclasses of the deprecated class will use the new class as superclass (optional)
 - rename all references in the code
 - move extensions of the deprecated class owned by other packages to the new class (optional)
 - remove the extensions (optional)

"
Class {
	#name : #RBApplyClassDeprecationRefactoring,
	#superclass : #RBClassRefactoring,
	#instVars : [
		'newName',
		'deprecatedClass',
		'shouldCopyExtensions',
		'shouldRemoveExtensions',
		'shouldFixSubclasses'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #'instance creation' }
RBApplyClassDeprecationRefactoring class >> model: aRBSmalltalk rename: aClass to: aNewName [ 
	^(self new)
		model: aRBSmalltalk;
		className: aClass name newName: aNewName;
		yourself
]

{ #category : #'instance creation' }
RBApplyClassDeprecationRefactoring class >> rename: aDeprecatedClass to: aNewName [
	^self new className: aDeprecatedClass name newName: aNewName
]

{ #category : #initialization }
RBApplyClassDeprecationRefactoring >> className: aName newName: aNewName [ 
	className := aName asSymbol.
	deprecatedClass := self model classNamed: className.
	newName := aNewName asSymbol
]

{ #category : #transforming }
RBApplyClassDeprecationRefactoring >> classSideExtensionMethodsOf: anRBClass [

	^ anRBClass realClass classSide localMethods 
		select: #isExtension 
		thenCollect: [ :each | anRBClass classSide methodFor: each selector ] 
]

{ #category : #transforming }
RBApplyClassDeprecationRefactoring >> copyExtensionMethods [

	(self instanceSideExtensionMethodsOf: deprecatedClass)
		do: [ :each | self newClass instanceSide compile: each source classified: each protocols ].
		
	(self classSideExtensionMethodsOf: deprecatedClass)
		do: [ :each | self newClass classSide compile: each source classified: each protocols ]
]

{ #category : #accessing }
RBApplyClassDeprecationRefactoring >> deprecatedClass [
	^ deprecatedClass
]

{ #category : #accessing }
RBApplyClassDeprecationRefactoring >> deprecatedClass: anObject [
	deprecatedClass := anObject
]

{ #category : #initialization }
RBApplyClassDeprecationRefactoring >> initialize [

	super initialize.
	
	shouldFixSubclasses := true.
	shouldCopyExtensions := true.
	shouldRemoveExtensions := true.
]

{ #category : #transforming }
RBApplyClassDeprecationRefactoring >> instanceSideExtensionMethodsOf: anRBClass [

	^ anRBClass realClass instanceSide localMethods 
		select: #isExtension 
		thenCollect: [ :each | anRBClass instanceSide methodFor: each selector ] 
]

{ #category : #transforming }
RBApplyClassDeprecationRefactoring >> newClass [

	^ self model classNamed: newName.  	
]

{ #category : #preconditions }
RBApplyClassDeprecationRefactoring >> preconditions [
	^((RBCondition withBlock: [deprecatedClass notNil and: [deprecatedClass isMeta not]]) 
			& (RBCondition isValidClassName: newName) 
			& (RBCondition isGlobal: newName in: self model)) |
			(RBCondition withBlock: [ self refactoringFailure: newName , ' is not a valid class name'])
]

{ #category : #transforming }
RBApplyClassDeprecationRefactoring >> removeExtensionMethods [

	(self instanceSideExtensionMethodsOf: deprecatedClass)
		do: [ :each | deprecatedClass instanceSide removeMethod: each selector ].
		
	(self classSideExtensionMethodsOf: deprecatedClass)
		do: [ :each | deprecatedClass classSide removeMethod: each selector ].

]

{ #category : #transforming }
RBApplyClassDeprecationRefactoring >> renameReferences [
	| replacer |
	replacer := (RBParseTreeRewriter replaceLiteral: className with: newName)
		replace: className with: newName;
		replaceArgument: newName
		withValueFrom: [:aNode | 
			self refactoringFailure: newName , ' already exists within the reference scope'];
		yourself.
				
	self model allReferencesToClass: deprecatedClass do: [ :method | 
		(method modelClass hierarchyDefinesVariable: newName) ifTrue: [
			self refactoringFailure: newName , ' is already defined in hierarchy of ', method modelClass printString].
		
		self convertMethod: method selector
			for: method modelClass
			using: replacer ]
]

{ #category : #transforming }
RBApplyClassDeprecationRefactoring >> renameSuperclassOfSubclasses [
	
	| replacement |
	replacement := self model classNamed: newName.  	
	self model reparentClasses: deprecatedClass subclasses to: replacement
]

{ #category : #accessing }
RBApplyClassDeprecationRefactoring >> shouldCopyExtensions [
	^ shouldCopyExtensions
]

{ #category : #accessing }
RBApplyClassDeprecationRefactoring >> shouldCopyExtensions: anObject [
	shouldCopyExtensions := anObject
]

{ #category : #accessing }
RBApplyClassDeprecationRefactoring >> shouldFixSubclasses [
	^ shouldFixSubclasses
]

{ #category : #accessing }
RBApplyClassDeprecationRefactoring >> shouldFixSubclasses: anObject [
	shouldFixSubclasses := anObject
]

{ #category : #accessing }
RBApplyClassDeprecationRefactoring >> shouldRemoveExtensions [
	^ shouldRemoveExtensions
]

{ #category : #accessing }
RBApplyClassDeprecationRefactoring >> shouldRemoveExtensions: anObject [
	shouldRemoveExtensions := anObject
]

{ #category : #printing }
RBApplyClassDeprecationRefactoring >> storeOn: aStream [ 
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' rename: '.
	deprecatedClass storeOn: aStream.
	aStream
		nextPutAll: ' to: #';
		nextPutAll: newName;
		nextPut: $)
]

{ #category : #transforming }
RBApplyClassDeprecationRefactoring >> transform [
	
	self shouldFixSubclasses 
		ifTrue: [ self renameSuperclassOfSubclasses ].

	self renameReferences.

	self shouldCopyExtensions 
		ifTrue: [ self copyExtensionMethods ].
		
	self shouldRemoveExtensions 
		ifTrue: [ self removeExtensionMethods ].

]
