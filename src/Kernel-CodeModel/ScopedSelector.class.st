Class {
	#name : 'ScopedSelector',
	#superclass : 'Object',
	#instVars : [
		'symbol',
		'owner',
		'extendedBehaviors',
		'aliasedBehaviors'
	],
	#category : 'Kernel-CodeModel-Extensions',
	#package : 'Kernel-CodeModel',
	#tag : 'Extensions'
}

{ #category : 'converting' }
ScopedSelector >> <= another [

	^ self asString <= another asString 
]

{ #category : 'installing' }
ScopedSelector >> addAliasedBehavior: aClass [

	aliasedBehaviors ifNil: [ aliasedBehaviors := Set new ].
	aliasedBehaviors add: aClass
]

{ #category : 'accessing' }
ScopedSelector >> addExtendedBehavior: aBehavior [

	extendedBehaviors ifNil: [ extendedBehaviors := Set new ].
	extendedBehaviors	add: aBehavior
]

{ #category : 'accessing' }
ScopedSelector >> aliasedBehaviors [

	^ aliasedBehaviors ifNil: [ #(  ) ]
]

{ #category : 'converting' }
ScopedSelector >> asSelector [
	
	^ self
]

{ #category : 'converting' }
ScopedSelector >> asString [

	^ self symbol asString
]

{ #category : 'converting' }
ScopedSelector >> asSymbol [
	
	^ self symbol
]

{ #category : 'matching' }
ScopedSelector >> beginsWith: aString [

	^ self asString beginsWith: aString
]

{ #category : 'accessing' }
ScopedSelector >> bindAliasIn: aClass [

	"Bind aliases in aClass using self as selector.
	
	Check how many methods to alias are there.
	If many extensions are available on the same class, with the same name => conflict.
	If there is an extension"

	| theAliasedMethod allVisibleExtensionsForThisClass locallyDefined methodsInConflict existingMethod isLegacyExtension |
	allVisibleExtensionsForThisClass := self visibleExtensionsOn: aClass.

	"Prevent conflicts!"
	existingMethod := aClass methodDict at: self symbol ifAbsent: [ nil ].
	isLegacyExtension := Extension allowMigration and: [existingMethod notNil and: [ existingMethod isExtension ] ].

	locallyDefined := (aClass includesSelector: self symbol) and: [ isLegacyExtension not ].
	methodsInConflict := locallyDefined
		ifTrue: [ 1 ]
		ifFalse: [ 0 ].
	methodsInConflict := methodsInConflict + allVisibleExtensionsForThisClass size.
	methodsInConflict > 1
		ifTrue: [ | conflictMethod | 
			conflictMethod := ConflictMethod new
				overwrittenSelector: self symbol;
				conflictingClass: aClass;
				conflictingExtensions: allVisibleExtensionsForThisClass;
				yourself.
		self addAliasedBehavior: aClass.
		aClass methodDict at: self put: conflictMethod.
		^ self
	].
	"From here we know there are maybe aliases to install but no conflicts."
	
	"If there are no extensions and no local methods, do nothing"
	(allVisibleExtensionsForThisClass isEmpty and: [ 
		(aClass includesSelector: self symbol) not])
			ifTrue: [ ^ self ].
	
	"Create an alias in aClass from `self` to the existing method with the same (global) name if any.
	If no method is there, no alias is created."
	theAliasedMethod := allVisibleExtensionsForThisClass
		ifEmpty: [ aClass >> self symbol ]
		ifNotEmpty: [ | theExtension theExtensionSelector |
			theExtension := allVisibleExtensionsForThisClass anyOne.
			theExtensionSelector := theExtension package visibleSelectorForSymbol: self symbol.
			aClass >> theExtensionSelector ].

	self addAliasedBehavior: aClass.
	aClass methodDict at: self put: theAliasedMethod
]

{ #category : 'accessing' }
ScopedSelector >> bindAliasesInSubclassesOf: aClass [

	"To be called after installing an extension method in aClass.
	Now all subclasses should be aliased: 
		that is, 
			for each subclass
			all methods with the same selector name as the new extension should include `self` as an alias to the existing method.
	
	This guarantees that polymorphism is kept between instances of the hierarchy regardless of the order in which messages are sent"

	aClass allSubclassesDo: [ :classToBind | self bindAliasIn: classToBind ]
]

{ #category : 'installing' }
ScopedSelector >> cleanupExtendedBehaviors [

	extendedBehaviors ifNil: [ ^ self ].
	extendedBehaviors do: [ :e | e methodDict removeKey: self ].
	extendedBehaviors := nil
]

{ #category : 'accessing' }
ScopedSelector >> extendedBehaviors [

	^ extendedBehaviors ifNil: [#()]
]

{ #category : 'accessing' }
ScopedSelector >> extensionsOn: aBehavior [

	| allVisibleExtensions |
	allVisibleExtensions := (owner definedClasses select: [ :e | e isKindOf: Extension ]) , owner importedExtensions.
	^ allVisibleExtensions select: [ :e | e extendedClass == aBehavior ]
]

{ #category : 'private' }
ScopedSelector >> flushCache [
	"Tell the virtual machine to remove all entries with this symbol as a selector from its method lookup caches, if it has any.  This 	must be done whenever a method is added, redefined or removed, so that message lookups reflect the revised organization.  c.f. 	Behavior>>flushCache & CompiledMethod>>flushCache.  Essential. See MethodDictionary class comment."

	<primitive: 119>
]

{ #category : 'accessing' }
ScopedSelector >> handleDoesNotUnderstand: aMessage to: aReceiver [

	"Lookup failed on a scoped send to a non-extended class.
	We should lookup an aliased method, and activate it."
	
	| aliasOwner |
	aliasOwner := self lookupAliasedMethodOnReceiverClass: aReceiver class lookupClass: aMessage lookupClass.
	
	"This looks like a real DNU"
	aliasOwner ifNil: [
		^ self symbol handleDoesNotUnderstand: aMessage to: aReceiver ].
	
	
	"We alias the class itself and its hierarchy"
	self bindAliasIn: aliasOwner.
	self bindAliasesInSubclassesOf: aliasOwner.
	
	^ aMessage sentTo: aReceiver
]

{ #category : 'converting' }
ScopedSelector >> isBinary [

	^ self symbol isBinary
]

{ #category : 'testing' }
ScopedSelector >> isDoIt [

	^ self symbol isDoIt
]

{ #category : 'testing' }
ScopedSelector >> isKeyword [

	^ self symbol isKeyword
]

{ #category : 'testing' }
ScopedSelector >> isScopedSelector [

	^ true
]

{ #category : 'accessing' }
ScopedSelector >> keywords [

	^ symbol keywords
]

{ #category : 'accessing' }
ScopedSelector >> localizedForPresenter: aSpLabelPresenter [

	^ self asString localizedForPresenter: aSpLabelPresenter
]

{ #category : 'accessing' }
ScopedSelector >> lookupAliasedMethodOnReceiverClass: aClass [

	^ self lookupAliasedMethodOnReceiverClass: aClass lookupClass: nil
]

{ #category : 'accessing' }
ScopedSelector >> lookupAliasedMethodOnReceiverClass: aClass lookupClass: lookupClass [

	| allVisibleExtensions theLookupClass |
	"
	Find the first class in the hierarchy (looking upwards) that defines a method with my name and is visible in my owner's scope.
	
	Methods can be locally defined, local extensions, or imported extensions."
	theLookupClass := lookupClass ifNil: [ aClass ].

	allVisibleExtensions := owner visibleExtensions.
	[ theLookupClass notNil ] whileTrue: [
			| allVisibleExtensionsForThisClass |
			
			"For each class, look for a normal method or an extension with my same name.
			If found, we need to alias from this class and its subclasses!"
			(theLookupClass includesSelector: self symbol) ifTrue: [ ^ theLookupClass ].
			
			"As long as there is one extension, it is enough here.
			The decision of installing an alias or a conflict will be taken later."
			allVisibleExtensionsForThisClass := allVisibleExtensions select: [ :e | e extendedClass == theLookupClass ].
			allVisibleExtensionsForThisClass ifNotEmpty: [ :es |
					| foundScopedMethod |
					foundScopedMethod := es anyOne extendedClass lookupSelector: (es anyOne package visibleSelectorForSymbol: self symbol).
					foundScopedMethod ifNotNil: [ ^ es anyOne extendedClass ] ].

			theLookupClass := theLookupClass superclass ].
	
	"We reached the top of the hierarchy and found no extensions"
	^ nil
]

{ #category : 'accessing' }
ScopedSelector >> numArgs [

	^ self symbol numArgs
]

{ #category : 'accessing' }
ScopedSelector >> owner [

	^ owner
]

{ #category : 'accessing' }
ScopedSelector >> owner: anObject [

	owner := anObject
]

{ #category : 'printing' }
ScopedSelector >> printOn: aStream [

	aStream
		nextPutAll: self class name;
		nextPutAll: '(';
		print: symbol;
		nextPutAll: ' - ';
		print: owner name;
		nextPutAll: ')'
]

{ #category : 'installing' }
ScopedSelector >> removeAliases [

	aliasedBehaviors ifNil: [ ^ self ].
	aliasedBehaviors do: [ :e | e removeSelector: self ].
	aliasedBehaviors := nil
]

{ #category : 'ston persistence' }
ScopedSelector >> stonOn: stonWriter [
	"Represent scoped selector as symbols when serialising"

	stonWriter writeObject: self listSingleton: self asSymbol
]

{ #category : 'accessing' }
ScopedSelector >> symbol [

	^ symbol
]

{ #category : 'accessing' }
ScopedSelector >> symbol: anObject [

	symbol := anObject asSymbol 
]

{ #category : 'accessing' }
ScopedSelector >> visibleExtensionsOn: aClass [

	"Return all extensions on aClass defining a selector with my same name and visible from my package"

	^ owner visibleExtensions select: [ :e |
		  e extendedClass == aClass and: [
				  | selectorInExtensionPackage |
				  selectorInExtensionPackage := e package visibleSelectorForSymbol: self symbol.
				  selectorInExtensionPackage isScopedSelector and: [ e includesSelector: selectorInExtensionPackage ] ] ]
]
