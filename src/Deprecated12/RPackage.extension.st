Extension { #name : 'RPackage' }

{ #category : '*Deprecated12' }
RPackage >> actualClassTags [

	self deprecated: 'This method is too specific and will be removed in future versions of Pharo.'.
	(classTags size = 1 and: [ classTags anyOne isRoot ]) ifTrue: [ ^ #(  ) ].

	^ classTags
]

{ #category : '*Deprecated12' }
RPackage >> addClassTag: tagName [

	self deprecated: 'Use #ensureTag: instead' transformWith: '`@rcv addClassTag: `@arg' -> '`@rcv ensureTag: `@arg'.
	^ self ensureTag: tagName
]

{ #category : '*Deprecated12' }
RPackage >> basicRemoveTag: tag [

	self deprecated: 'Use #removeTag: instead.' transformWith: '`@rcv removeClassTag: `@arg' -> '`@rcv removeTag: `@arg'.
	self removeTag: tag
]

{ #category : '*Deprecated12' }
RPackage >> classDefinedSlicesDo: aBlock [

	self deprecated:
		'The API of RPackage is been cleaned and this method is not generic enough to stay in RPackage. You can look at the implementation of this method to know how to reproduce its behavior.'.
	self definedClasses
		reject: [ :class | class isMeta ]
		thenDo: [ :class | (self definedSelectorsForClass: class) ifNotEmpty: [ :selectors | aBlock value: class name value: selectors ] ]
]

{ #category : '*Deprecated12' }
RPackage >> classExtensionSlicesDo: aBlock [
	"This method iterates over the class extensions and their associated selectors. A slice is a class * list of selectors. aBlock will be applied to all the extensions slices of the receiver. aBlock first argument is the class and the second argument a list of method selectors"

	self deprecated:
		'The API of RPackage is been cleaned and this method is not generic enough to stay in RPackage. You can look at the implementation of this method to know how to reproduce its behavior.'.

	self extendedClasses
		reject: [ :class | class isMeta ]
		thenDo: [ :class | (self extensionSelectorsForClass: class) ifNotEmpty: [ :selectors | aBlock value: class name value: selectors ] ]
]

{ #category : '*Deprecated12' }
RPackage >> classNamesAndExtensionSelectorsDo: aBlock [
	"Iterate over the extended methods grouped by classes and apply the argument.
	The block will be passed a class name and each individual selectors.
	Only classes with methods are paired with their methods"

	self deprecated:
		'The API of RPackage is been cleaned and this method is not generic enough to stay in RPackage. You can look at the implementation of this method to know how to reproduce its behavior.'.
	self extendedClasses do: [ :class |
		(self extensionSelectorsForClass: class) ifNotEmpty: [ :selectors | selectors do: [ :selector | aBlock value: class name value: selector ] ] ]
]

{ #category : '*Deprecated12' }
RPackage >> classNamesForClassTag: aSymbol [
	"Returns the classes tagged using aSymbol"

	self deprecated: 'This method is too specific and will be remove in future versions of Pharo. If you are using it you can inline the method'.

	^ (self classTagNamed: aSymbol ifAbsent: [ ^ #(  ) ]) classNames
]

{ #category : '*Deprecated12' }
RPackage >> classTagForClass: aClass [

	self deprecated: 'Use #tagOf: instead' transformWith: '`@rcv classTagForClass: `@arg' -> '`@rcv tagOf: `@arg'.
	^ self tagOf: aClass
]

{ #category : '*Deprecated12' }
RPackage >> classesForClassTag: aTagName [

	self deprecated: 'Use #classesTaggedWith: instead.' transformWith: '`@rcv classesForClassTag: `@arg' -> '`@rcv classesTaggedWith: `@arg'.
	^ self classesTaggedWith: aTagName
]

{ #category : '*Deprecated12' }
RPackage >> definedClassesDo: aBlock [

	self deprecated:
		'Use #definedClasses and a do instead because the name of this method is not explicit since it iterates over the *name* of the classes and not the classes themselves.'.
	^ self definedClassNames do: aBlock
]

{ #category : '*Deprecated12' }
RPackage >> extensionCategoriesForClass: aClass [

	self
		deprecated: 'Use #extensionProtocolsForClass: instead.'
		transformWith: '`@rcv extensionCategoriesForClass: `@arg' -> '`@rcv extensionProtocolsForClass: `@arg'.
	^ self extensionProtocolsForClass: aClass
]

{ #category : '*Deprecated12' }
RPackage >> includesDefinedSelector: aSelector ofClassName: aClassName [
	"Return true if the receiver includes the method of selector aSelector. Only checks methods defined (not extended by other packages or package extensions)"

	self deprecated: 'Use #includesDefinedSelector:ofClass: with the class directly'.
	^ self includesDefinedSelector: aSelector ofClass: (self environment at: aClassName)
]

{ #category : '*Deprecated12' }
RPackage >> includesDefinedSelector: aSelector ofMetaclassName: aClassName [
	"Return true if the receiver includes the method of selector aSelector. Only checks methods defined (not extended by other packages or package extensions)"

	self deprecated: 'Use #includesDefinedSelector:ofClass: with the class side of the class directly'.
	^ self includesDefinedSelector: aSelector ofClass: (self environment at: aClassName) class
]

{ #category : '*Deprecated12' }
RPackage >> includesExtensionSelector: aSelector ofClassName: aClassName [
	"Return true if the receiver includes the method of selector aSelector. Only checks methods extending other packages"

	self deprecated: 'Use #includesExtensionSelector:ofClass: with the class directly'.
	^ self includesExtensionSelector: aSelector ofClass: (self environment at: aClassName)
]

{ #category : '*Deprecated12' }
RPackage >> includesExtensionSelector: aSelector ofMetaclassName: aClassName [
	"Return true if the receiver includes the method of selector aSelector. Only checks methods extending other packages"

	self deprecated: 'Use #includesExtensionSelector:ofClass: with the class side of the class directly'.
	^ self includesExtensionSelector: aSelector ofClass: (self environment at: aClassName) class
]

{ #category : '*Deprecated12' }
RPackage >> includesSelector: aSelector ofClassName: aClassName [
	"Return true if the receiver includes the method of selector aSelector. Checks methods defined locally as well as extending other packages"

	self deprecated: 'Use #includesSelector:ofClass: directly with the class.'.
	^ self includesSelector: aSelector ofClass: (self environment at: aClassName)
]

{ #category : '*Deprecated12' }
RPackage >> includesSelector: aSelector ofMetaclassName: aClassName [
	"Return true if the receiver includes the method of selector aSelector. Checks methods defined locally as well as extending other packages"

	self deprecated: 'Use #includesSelector:ofClass: directly with the class.'.
	^ self includesSelector: aSelector ofClass: (self environment at: aClassName)
]

{ #category : '*Deprecated12' }
RPackage >> isYourClassExtension: protocol [

	self
		deprecated: 'Use #isExtensionMatching: on the parameter instead because the name is much more explicit.'
		transformWith: '`@rcv isYourClassExtension: `@arg' -> '`@arg isExtensionProtocolMatching: `@rcv'.

	^ protocol isExtensionProtocolMatching: self
]

{ #category : '*Deprecated12' }
RPackage >> metaclassDefinedSlicesDo: aBlock [
	"This method iterates over the defined class and their associated selectors. a slice is a class * list of selectors. aBlock will be applied to all the extensions slices of the receiver. aBlok first argument is the class and the second argument a list of method selectors"

	self deprecated:
		'The API of RPackage is been cleaned and this method is not generic enough to stay in RPackage. You can look at the implementation of this method to know how to reproduce its behavior.'.

	self definedClasses
		select: [ :class | class isMeta ]
		thenDo: [ :class | (self definedSelectorsForClass: class) ifNotEmpty: [ :selectors | aBlock value: class instanceSide name value: selectors ] ]
]

{ #category : '*Deprecated12' }
RPackage >> metaclassExtensionSlicesDo: aBlock [
	"This method iterates over the metaclass extensions and their associated selectors. A slice is a class * list of selectors. aBlock will be applied to all the extensions slices of the receiver. aBlok first argument is the class and second argument a list of selectors"

	self deprecated:
		'The API of RPackage is been cleaned and this method is not generic enough to stay in RPackage. You can look at the implementation of this method to know how to reproduce its behavior.'.

	self extendedClasses
		select: [ :class | class isMeta ]
		thenDo: [ :class | (self extensionSelectorsForClass: class) ifNotEmpty: [ :selectors | aBlock value: class instanceSide name value: selectors ] ]
]

{ #category : '*Deprecated12' }
RPackage >> removeClassNamed: aClassName [

	self deprecated: 'Use #removeClass: with a real class instead.'.
	^ self removeClass: (self organizer environment at: aClassName)
]

{ #category : '*Deprecated12' }
RPackage >> removeClassTag: aSymbol [

	self deprecated: 'Use #removeTag: instead.' transformWith: '`@rcv removeClassTag: `@arg' -> '`@rcv removeTag: `@arg'.
	self removeTag: aSymbol
]
