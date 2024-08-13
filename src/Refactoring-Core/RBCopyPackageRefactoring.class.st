"
I am copy a package (giving it a new name) and the classes it contains.

My preconditions verify that
- the copied package exists in  the current environment and 
- the new copy package name is valid and not yet used as a global variable name.

The refactoring transformation creates a new package and copy the classes of the origina package with 
a new name prefixed with Copy. Pay attention the class extensions are not copied.

#### Example

```
(RBCopyPackageRefactoring 
	copyPackage: #'Beacon-Core' 
	in: #'Beacon-Core1') execute. 
```
"
Class {
	#name : 'RBCopyPackageRefactoring',
	#superclass : 'RBPackageRefactoring',
	#instVars : [
		'package',
		'classMappings'
	],
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'copying' }
RBCopyPackageRefactoring class >> copyPackage: aString1 in: aString2 [
	^ self new
		copyPackage: aString1 in: aString2;
		yourself
]

{ #category : 'copying' }
RBCopyPackageRefactoring class >> model: aRBSmalltalk copyPackage: aString1 in: aString2 [
	^ self new
		model: aRBSmalltalk;
		copyPackage: aString1 in: aString2;
		yourself
]

{ #category : 'preconditions' }
RBCopyPackageRefactoring >> applicabilityPreconditions [ 

	| conds |
	conds := super applicabilityPreconditions.
	^ conds , {
		 (RBCondition withBlock: [ newName ~= packageName ] errorString: 'The new package name is the same as the old package name.').
		 (RBCondition
			   withBlock: [ "Cyril: I am not sure we should use #packageOrganizer. Maybe we should ask the environment the package manager. But currently the image does not know yet how to work with multiple package managers/modules."
				   (self packageOrganizer hasPackage: newName) not ]
			   errorString: 'The system already includes a package named ' , newName) }
]

{ #category : 'preconditions' }
RBCopyPackageRefactoring >> changeReferencesOf: classes with: copyClasses [

	[ : job |
		| jobIndex |
		jobIndex := 1.
		job max: classes size.
		classes with: copyClasses do: [ :cls :copiedClass |
			| rbClass |
			job
				currentValue: jobIndex;
				title: 'Changing reference of ' , copiedClass printString.
			rbClass := self model classNamed: cls.
			self renameReferencesOf: rbClass with: copiedClass.
			jobIndex := jobIndex + 1 ]
	] asJob run
]

{ #category : 'accessing' }
RBCopyPackageRefactoring >> classMappings [

	^ classMappings
]

{ #category : 'accessing' }
RBCopyPackageRefactoring >> classMappings: aDictionary [
	"Set the receiver's dictionary containing mappings between the source class name to be copied and the target new namename"

	classMappings := aDictionary
]

{ #category : 'preconditions' }
RBCopyPackageRefactoring >> classes [
	"Answer a <Collection> of the receiver's class names to be copied"

	^ self classMappings keys
]

{ #category : 'preconditions' }
RBCopyPackageRefactoring >> copyAllClasses [
	"Perform a copy class refactoring on each new class to be copied and answer a <Collection> with the names of the new"

	^ self classes collect: [ :symbol |
		| copyClassName |
		copyClassName := (self classMappings at: symbol) asSymbol.
		self generateChangesFor:
			(ReCopyClassRefactoring
				model: self model
				copyClass: symbol
				withName: copyClassName
				in: newName).
		copyClassName ]
]

{ #category : 'copying' }
RBCopyPackageRefactoring >> copyPackage: aString1 in: aString2 [
	packageName := aString1 asSymbol.
	package := self model packageNamed: packageName.
	newName := aString2 asSymbol
]

{ #category : 'transforming' }
RBCopyPackageRefactoring >> privateTransform [
	| copyClasses |

	self model addPackageNamed: newName.
	copyClasses := self copyAllClasses.
	self changeReferencesOf: self classes with: copyClasses.
	self reparent: self classes with: copyClasses
]

{ #category : 'renaming' }
RBCopyPackageRefactoring >> renameReferencesOf: aClass1 with: aClass2 [
	| replacer |
	replacer := (self parseTreeRewriterClass replaceLiteral: aClass1 name with: aClass2)
				replace: aClass1 name with: aClass2;
				replaceArgument: aClass2
					withValueFrom:
						[:aNode |
						self
							refactoringError: aClass2 , ' already exists within the reference scope'];
				yourself.
	self model allReferencesToClass: aClass1 inPackages: { newName }
		do:
			[:method |
			(method modelClass hierarchyDefinesVariable: aClass2)
				ifTrue:
					[self refactoringError: aClass2 , ' is already defined in hierarchy of '
								, method modelClass printString].
			self
				convertMethod: method selector
				for: method modelClass
				using: replacer]
]

{ #category : 'preconditions' }
RBCopyPackageRefactoring >> reparent: classes  with: copyClasses [

	[ : job | | jobIndex subclasses dict |
		jobIndex := 1.
		dict := Dictionary newFromKeys: classes andValues: copyClasses.
		subclasses := copyClasses
			collect: [ :cls | self model classNamed: cls]
			thenSelect: [ :rb | classes includes: rb superclass name ].
		subclasses do: [ :cls |
			self model reparentClasses: { cls } to: (self model classNamed: (dict at: cls superclass name)).
			job
				currentValue: jobIndex;
				title: (String streamContents: [ : stream | stream << 'Reparent class '; << cls printString ]).
			jobIndex := jobIndex + 1 ]
		] asJob run
]

{ #category : 'storing' }
RBCopyPackageRefactoring >> storeOn: aStream [
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' copyPackage: '.
	aStream nextPutAll: package name.
	aStream
		nextPutAll: ' in: #';
		nextPutAll: newName;
		nextPut: $)
]
