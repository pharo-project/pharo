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
	#name : #RBDeprecateClassRefactoring,
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
RBDeprecateClassRefactoring class >> deprecate: aDeprecatedClass in: aNewName [
	^ self new className: aDeprecatedClass name newName: aNewName
]

{ #category : #'instance creation' }
RBDeprecateClassRefactoring class >> model: aRBSmalltalk deprecate: aClass in: aNewName [ 
	^ self new
		model: aRBSmalltalk;
		className: aClass name newName: aNewName;
		yourself
]

{ #category : #initialization }
RBDeprecateClassRefactoring >> className: aName newName: aNewName [ 
	className := aName asSymbol.
	deprecatedClass := self model classNamed: className.
	newName := aNewName asSymbol
]

{ #category : #transforming }
RBDeprecateClassRefactoring >> classSideExtensionMethodsOf: anRBClass [

	^ anRBClass realClass classSide localMethods 
		select: #isExtension 
		thenCollect: [ :each | anRBClass classSide methodFor: each selector ] 
]

{ #category : #transforming }
RBDeprecateClassRefactoring >> convertDeprecatedToSubclass [
	| replacement |
	replacement := self model classNamed: newName.
	self model reparentClasses: {deprecatedClass} to: replacement.
	deprecatedClass selectors do: [ :symbol | 
		(deprecatedClass realClass instanceSide compiledMethodAt: symbol) isExtension 
			ifFalse: [ deprecatedClass removeMethod: symbol ] ].
	deprecatedClass classSide selectors do: [ :symbol | 
		(deprecatedClass realClass classSide compiledMethodAt: symbol) isExtension
			ifFalse: [ deprecatedClass classSide removeMethod: symbol ] ].
	deprecatedClass classSide compile: 'isDeprecated
		" Uses ', newName, ' instead of ', className,'"
		^ true' classified: {'testing'}
]

{ #category : #transforming }
RBDeprecateClassRefactoring >> copyExtensionMethods [

	(self instanceSideExtensionMethodsOf: deprecatedClass)
		do: [ :each | self newClass instanceSide compile: each source classified: each protocols ].
		
	(self classSideExtensionMethodsOf: deprecatedClass)
		do: [ :each | self newClass classSide compile: each source classified: each protocols ]
]

{ #category : #accessing }
RBDeprecateClassRefactoring >> deprecatedClass [
	^ deprecatedClass
]

{ #category : #accessing }
RBDeprecateClassRefactoring >> deprecatedClass: anObject [
	deprecatedClass := anObject
]

{ #category : #initialization }
RBDeprecateClassRefactoring >> initialize [

	super initialize.
	
	shouldFixSubclasses := true.
	shouldCopyExtensions := true.
	shouldRemoveExtensions := true.
]

{ #category : #transforming }
RBDeprecateClassRefactoring >> instanceSideExtensionMethodsOf: anRBClass [

	^ anRBClass realClass instanceSide localMethods 
		select: #isExtension 
		thenCollect: [ :each | anRBClass instanceSide methodFor: each selector ] 
]

{ #category : #transforming }
RBDeprecateClassRefactoring >> newClass [

	^ self model classNamed: newName.  	
]

{ #category : #preconditions }
RBDeprecateClassRefactoring >> preconditions [
	^((RBCondition withBlock: [deprecatedClass notNil and: [deprecatedClass isMeta not]]) 
			& (RBCondition isValidClassName: newName) 
			& (RBCondition isGlobal: newName in: self model)) |
			(RBCondition withBlock: [ self refactoringFailure: newName , ' is not a valid class name'])
]

{ #category : #transforming }
RBDeprecateClassRefactoring >> removeExtensionMethods [

	(self instanceSideExtensionMethodsOf: deprecatedClass)
		do: [ :each | deprecatedClass instanceSide removeMethod: each selector ].
		
	(self classSideExtensionMethodsOf: deprecatedClass)
		do: [ :each | deprecatedClass classSide removeMethod: each selector ].

]

{ #category : #transforming }
RBDeprecateClassRefactoring >> renameReferences [
	| replacer |
	replacer := (self parseTreeRewriterClass replaceLiteral: className with: newName)
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
RBDeprecateClassRefactoring >> renameSuperclassOfSubclasses [
	
	| replacement |
	replacement := self model classNamed: newName.  	
	self model reparentClasses: deprecatedClass subclasses to: replacement
]

{ #category : #accessing }
RBDeprecateClassRefactoring >> shouldCopyExtensions [
	^ shouldCopyExtensions
]

{ #category : #accessing }
RBDeprecateClassRefactoring >> shouldCopyExtensions: anObject [
	shouldCopyExtensions := anObject
]

{ #category : #accessing }
RBDeprecateClassRefactoring >> shouldFixSubclasses [
	^ shouldFixSubclasses
]

{ #category : #accessing }
RBDeprecateClassRefactoring >> shouldFixSubclasses: anObject [
	shouldFixSubclasses := anObject
]

{ #category : #accessing }
RBDeprecateClassRefactoring >> shouldRemoveExtensions [
	^ shouldRemoveExtensions
]

{ #category : #accessing }
RBDeprecateClassRefactoring >> shouldRemoveExtensions: anObject [
	shouldRemoveExtensions := anObject
]

{ #category : #printing }
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

{ #category : #transforming }
RBDeprecateClassRefactoring >> transform [
	
	self shouldFixSubclasses 
		ifTrue: [ self renameSuperclassOfSubclasses.
			self convertDeprecatedToSubclass ].

	self renameReferences.

	self shouldCopyExtensions 
		ifTrue: [ self copyExtensionMethods ].
		
	self shouldRemoveExtensions 
		ifTrue: [ self removeExtensionMethods ].

]
