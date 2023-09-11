Class {
	#name : #AbstractPackage,
	#superclass : #Object,
	#instVars : [
		'organizer'
	],
	#category : #'RPackage-Core-Base'
}

{ #category : #comparing }
AbstractPackage >> <= aRPackage [

	^ self name <= aRPackage name
]

{ #category : #'adding-removing' }
AbstractPackage >> addClass: aClass [

	self subclassResponsibility
]

{ #category : #'adding-removing' }
AbstractPackage >> addMethod: aCompiledMethod [

	self subclassResponsibility
]

{ #category : #'class tags' }
AbstractPackage >> classTagCategoryNames [

	^ (Set with: self name) , (self classTags collect: [ :each | each categoryName ])
]

{ #category : #'class tags' }
AbstractPackage >> classTagNamed: aSymbol [

	^ self classTags detect: [ :each | each name = aSymbol ]
]

{ #category : #'class tags' }
AbstractPackage >> classTagNamed: aSymbol ifAbsent: aBlock [

	^ self classTags
		  detect: [ :each | each name = aSymbol ]
		  ifNone: aBlock
]

{ #category : #'class tags' }
AbstractPackage >> classTagNamed: aSymbol ifPresent: aBlock [

	^ self classTags
		  detect: [ :each | each name = aSymbol ]
		  ifFound: [ :tag | aBlock cull: tag ]
		  ifNone: [ nil ]
]

{ #category : #'class tags' }
AbstractPackage >> classTags [

	^ self subclassResponsibility
]

{ #category : #accessing }
AbstractPackage >> classes [

	^ self subclassResponsibility
]

{ #category : #'class tags' }
AbstractPackage >> classesTaggedWith: aSymbol [
	"Returns the classes tagged using aSymbol"

	^ (self classTagNamed: aSymbol ifAbsent: [ ^ #(  ) ]) classes
]

{ #category : #accessing }
AbstractPackage >> definedClassNames [
	"Return the class names having methods defined in the receiver."

	^ self definedClasses collect: [ :class | class name ]
]

{ #category : #accessing }
AbstractPackage >> definedClasses [

	^ self subclassResponsibility
]

{ #category : #accessing }
AbstractPackage >> definedMethods [

	^ self subclassResponsibility
]

{ #category : #accessing }
AbstractPackage >> definedMethodsForClass: aClass [

	^ (self definedSelectorsForClass: aClass) asOrderedCollection collect: [ :each | aClass >> each ]
]

{ #category : #accessing }
AbstractPackage >> definedOrExtendedClasses [
	^ self definedClasses | self extendedClasses
]

{ #category : #accessing }
AbstractPackage >> definedSelectors [

	^ self subclassResponsibility
]

{ #category : #accessing }
AbstractPackage >> definedSelectorsForClass: aClass [

	^ self subclassResponsibility
]

{ #category : #converting }
AbstractPackage >> demoteToTagInPackage [

	| newPackage tag |
	(self name includes: $-) ifFalse: [ self error: 'To demote a package, it name needs to contain at least one dash `-`.' ].

	self flag: #package. "We need to unregister because we cannot have a package X-Y and a package X with a tag Y at the same time because of the system organizer. Once the system organizer is not here anymore, we should just do a #removeFromSystem at the end to avoid to have a removal system AND an unregistering system."
	self unregister.

	newPackage := self organizer ensurePackage: (self name copyUpToLast: $-).

	"We keep the suffix that was removed as the tag name to create."
	tag := newPackage ensureTag: (self name withoutPrefix: newPackage name , '-').

	self definedClasses do: [ :class | newPackage moveClass: class toTag: tag ].
	self extensionMethods do: [ :method | newPackage addMethod: method ].

	^ newPackage
]

{ #category : #'class tags' }
AbstractPackage >> ensureTag: aTag [

	self subclassResponsibility
]

{ #category : #accessing }
AbstractPackage >> environment [

	^ self organizer environment
]

{ #category : #accessing }
AbstractPackage >> extendedClassNames [
	"Return the name of the classes which are extended by the receiver package. if a metaclass is extended, just get its sole instance class name."

	^ self extendedClasses collect: [ :class | class instanceSide name ] as: Set
]

{ #category : #accessing }
AbstractPackage >> extendedClasses [

	^ self subclassResponsibility
]

{ #category : #accessing }
AbstractPackage >> extensionMethods [

	^ self subclassResponsibility
]

{ #category : #accessing }
AbstractPackage >> extensionMethodsForClass: aClass [
	"Change the set of extensions selectors to an Array to avoid compiled methods collisions in the resulting set."

	^ (self extensionSelectorsForClass: aClass) asArray collect: [ :each | aClass >> each ]
]

{ #category : #accessing }
AbstractPackage >> extensionSelectors [

	^ self subclassResponsibility
]

{ #category : #accessing }
AbstractPackage >> extensionSelectorsForClass: aClass [

	^ self subclassResponsibility
]

{ #category : #testing }
AbstractPackage >> hasTag: aTag [
	"Takes a package tag or a package tag name as parameter and return true if I include this package tag."

	^ self tagNames includes: (aTag isString
			   ifTrue: [ aTag ]
			   ifFalse: [ aTag name ])
]

{ #category : #accessing }
AbstractPackage >> hierarchyRoots [
	"Returns all the hiearchy roots (by opposition to single classes inheriting from class outside the package) of a package"

	^ self definedClasses select: [ :each | (each superclass isNil or: [ each superclass package ~~ self ]) and: [ each hasSubclasses ] ]
]

{ #category : #testing }
AbstractPackage >> includesDefinedSelector: aSelector ofClass: aClass [
	"Return true if the receiver includes the method of selector aSelector. Only checks methods defined in this package"

	^ (self definedSelectorsForClass: aClass) includes: aSelector asSymbol
]

{ #category : #testing }
AbstractPackage >> isEmpty [

	^ self classes isEmpty and: [ self extensionSelectors isEmpty ]
]

{ #category : #testing }
AbstractPackage >> isTestPackage [
	"1. Test package ends with suffix -Tests. Suffix is case sensitive.
	 2. Or test package contains '-Tests-' in middle.
	Some examples: "
	"(RPackage named: 'MockPackage-Tests') isTestPackage >>> true"
	"(RPackage named: 'MockPackage-tests') isTestPackage >>> false"
	"(RPackage named: 'MockPackage') isTestPackage >>> false"
	"(RPackage named: 'MockPackage-Tests-Package') isTestPackage >>> true"

	^ (self name endsWith: '-Tests') or: [self name includesSubstring: '-Tests-']
]

{ #category : #accessing }
AbstractPackage >> linesOfCode [
	"An approximate measure of lines of code.
	Includes comments, but excludes blank lines."
	^self methods inject: 0 into: [:sum :each | sum + each linesOfCode]
]

{ #category : #accessing }
AbstractPackage >> methods [
	"Return all the methods defined in this package. Including extension methods (i.e., methods defined on a class that is not defined by me)"

	^ self definedMethods , self extensionMethods
]

{ #category : #accessing }
AbstractPackage >> methodsForClass: aClass [
	"Returns all compiled methods for a class in the receiver without distinction of extensions or not"

	^ (self includesClass: aClass)
		ifFalse: [self extensionMethodsForClass: aClass]
		ifTrue: [self definedMethodsForClass: aClass]
]

{ #category : #accessing }
AbstractPackage >> name [

	^ self subclassResponsibility
]

{ #category : #private }
AbstractPackage >> organizer [

	self flag: #package.
	^ organizer ifNil: [ self class organizer ]
]

{ #category : #accessing }
AbstractPackage >> organizer: anObject [

	organizer := anObject
]

{ #category : #accessing }
AbstractPackage >> packageManifestOrNil [
	^ self definedClasses
		detect: [ :each | each isManifest ]
		ifNone: [ nil ]
]

{ #category : #accessing }
AbstractPackage >> packageName [

	self flag: #package. "Deprecate and call #name instead."
	^ self name
]

{ #category : #printing }
AbstractPackage >> printOn: aStream [
	super printOn: aStream.
	aStream nextPut: $(.
	aStream nextPutAll: self name.
	aStream nextPut: $)
]

{ #category : #'adding-removing' }
AbstractPackage >> removeClass: aClass [

	self subclassResponsibility
]

{ #category : #'class tags' }
AbstractPackage >> removeEmptyTags [

	(self classTags select: [ :tag | tag isEmpty ]) do: [ :emptyTag | self removeTag: emptyTag ]
]

{ #category : #'adding-removing' }
AbstractPackage >> removeFromSystem [

	self subclassResponsibility
]

{ #category : #'adding-removing' }
AbstractPackage >> removeMethod: aCompiledMethod [

	self subclassResponsibility
]

{ #category : #'adding-removing' }
AbstractPackage >> removeMethods: aCollection [
	aCollection do: [ :each | self removeMethod: each ]
]

{ #category : #'adding-removing' }
AbstractPackage >> removeTag: aTag [

	self subclassResponsibility
]

{ #category : #'class tags' }
AbstractPackage >> renameTag: aTag to: newName [

	(self hasTag: aTag) ifFalse: [ ^ self ].

	(aTag isString
		 ifTrue: [ self classTagNamed: aTag ]
		 ifFalse: [ aTag ]) renameTo: newName
]

{ #category : #accessing }
AbstractPackage >> rootTag [

	^ self subclassResponsibility
]

{ #category : #accessing }
AbstractPackage >> rootTagName [

	self flag: #package. "In the future we should not use the name of the package because it causes bugs."
	^ self name
]

{ #category : #accessing }
AbstractPackage >> roots [
	"Returns all the root classes of a package. A root class is a class whose superclass is not defined in the package.
	Root classes are potentially root of inheritance trees defined in a package."

	^ self definedClasses
		select: [ :each | each superclass isNil or: [ each superclass package ~~ self ] ]
]

{ #category : #accessing }
AbstractPackage >> selectors [

	^ self definedSelectors asSet
		  addAll: self extensionSelectors;
		  yourself
]

{ #category : #accessing }
AbstractPackage >> selectorsForClass: aClass [
	"Returns all selectors for a class in the receiver without distinction of extensions or not"

	^ (self includesClass: aClass)
		ifFalse: [self extensionSelectorsForClass: aClass]
		ifTrue: [self definedSelectorsForClass: aClass]
]

{ #category : #'class tags' }
AbstractPackage >> tagNames [
	^ self classTags collect: [ :tag | tag name ]
]

{ #category : #'class tags' }
AbstractPackage >> tagOf: aClass [

	^ self subclassResponsibility
]
