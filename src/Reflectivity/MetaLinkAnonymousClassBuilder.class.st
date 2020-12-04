"
I build anonymous subclasses for object with instance specific MetaLinks. 

I can compile methods in those subclasses, and provide access to anonymous classes;

I also handle the migration of an object from its original class to an anonymous subclass and vice versa.

I consider that for one anonymous subclass i hold one object reference. I therefore cannot work as is with other clients using anonymous subclasses.
"
Class {
	#name : #MetaLinkAnonymousClassBuilder,
	#superclass : #Object,
	#instVars : [
		'classes',
		'migratedObjects'
	],
	#category : #'Reflectivity-Installer'
}

{ #category : #accessing }
MetaLinkAnonymousClassBuilder >> allSubclassesOf: class withSelector: selector [
	| subclasses |
	class isAnonymous
		ifTrue: [ ^ OrderedCollection with: class ].
	subclasses := self anonSubclassesFor: class.
	^ (subclasses select: [ :c | c selectors includes: selector ]) asOrderedCollection
]

{ #category : #accessing }
MetaLinkAnonymousClassBuilder >> anonSubclassesFor: aClass [
	^ classes at: aClass ifAbsent: [ ^ Array new ]
]

{ #category : #creation }
MetaLinkAnonymousClassBuilder >> anonymousClassForObject: anObject [
	"Building the anonymous subclass for an object.
	If the object already is instance of an anonymous class, its class is returned as is.
	Otherwise an anonymous class is derived from its class.
	There is a single anonymous class by adapted object (1-1 relationship)."

	| class |
	class := anObject class.
	^ class isAnonymous
		ifTrue: [ class ]
		ifFalse: [ self newAnonymousSubclassFor: class ]
]

{ #category : #accessing }
MetaLinkAnonymousClassBuilder >> classAccessorsForAnonymousClasses [
	^{'class
		thisContext sender methodClass == MetaLinkAnonymousClassBuilder 
			ifTrue:[^self realClass].
		^self originalClass'.
		
	'originalClass
		^super class superclass'.
		
	'realClass
		^super class'				
	}
]

{ #category : #compiling }
MetaLinkAnonymousClassBuilder >> compileClassAccessorForAnonymousClass: anAnonymousClass [
	self classAccessorsForAnonymousClasses
		do: [ :sourceCode | anAnonymousClass compile: sourceCode ]
]

{ #category : #compiling }
MetaLinkAnonymousClassBuilder >> compileMethodFrom: aNode in: anAnonymousClass [
	| selector source |
	selector := aNode methodNode selector.
	(anAnonymousClass methodDict at: selector ifAbsent: [ nil ]) ifNotNil: [ :compiledMethod | ^ compiledMethod ].
	source := aNode methodNode source.
	anAnonymousClass compile: source.	
	^ anAnonymousClass >> selector
]

{ #category : #accessing }
MetaLinkAnonymousClassBuilder >> compiledMethodsOfSelector: selector inAnonSubClassesOf: class [
	| anonSubClasses |
	anonSubClasses := self anonSubclassesFor: class.
	^ self compiledMethodsOfSelector: selector inClasses: anonSubClasses
]

{ #category : #accessing }
MetaLinkAnonymousClassBuilder >> compiledMethodsOfSelector: selector inClasses: someClasses [
	^ (someClasses select: [ :ac | ac selectors includes: selector ])
		collect: [ :ac | ac compiledMethodAt: selector ]
]

{ #category : #initialize }
MetaLinkAnonymousClassBuilder >> initialize [
	classes := Dictionary new.
	migratedObjects := WeakIdentityKeyDictionary new
]

{ #category : #migration }
MetaLinkAnonymousClassBuilder >> migrateObject: anObject toAnonymousClass: anonClass [
	anObject class == anonClass
		ifTrue: [ ^ self ].
	anonClass adoptInstance: anObject.
	migratedObjects at: anonClass put: (WeakArray with: anObject)
]

{ #category : #migration }
MetaLinkAnonymousClassBuilder >> migrateObjectToOriginalClass: anObject [
	| class |
	class := anObject class.
	class isAnonymous
		ifFalse: [ ^ self ].
	migratedObjects removeKey: class.
	class superclass adoptInstance: anObject
]

{ #category : #creation }
MetaLinkAnonymousClassBuilder >> newAnonymousSubclassFor: aClass [
	| anonSubclass |
	anonSubclass := aClass newAnonymousSubclass.
	(classes at: aClass ifAbsentPut: WeakSet new)
		add: anonSubclass.
	self compileClassAccessorForAnonymousClass: anonSubclass.
	^ anonSubclass
]

{ #category : #creation }
MetaLinkAnonymousClassBuilder >> removeMethodNode: aNode fromObject: anObject [
	anObject class isAnonymous ifFalse:[^self].
	anObject class removeSelector: aNode methodNode selector.

]

{ #category : #accessing }
MetaLinkAnonymousClassBuilder >> soleInstanceOf: anAnonymousClass [
	| weakArray |
	anAnonymousClass
		ifNil: [ 
			"Because anonymous subclasses are weakly referenced by the builder,
			we assume other tools could do the same and ask for the sole instance
			of a nilled reference (e.g., recovered from a weak array).
			In that case (anAnonymousClass isNil), we want to avoid to look into
			the registry because it also holds weak references to anonymous classes.
			It would then find a nil key associated to a nil object, and produce a
			ValueNotFound error while we'rere expecting to be warn that the anonymous
			class is not there anymore (KeyNotFound)."
			KeyNotFound signalFor: anAnonymousClass in: migratedObjects ].
	weakArray := migratedObjects at: anAnonymousClass.
	(weakArray isEmpty or: [ weakArray first isNil ])
		ifTrue: [ ValueNotFound
				signalFor: anAnonymousClass
				in: (migratedObjects at: anAnonymousClass) ].
	^ weakArray first
]
