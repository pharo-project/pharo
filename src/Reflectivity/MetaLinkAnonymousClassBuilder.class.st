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
	migratedObjects := WeakKeyDictionary new
]

{ #category : #migration }
MetaLinkAnonymousClassBuilder >> migrateObject: anObject toAnonymousClass: anonClass [
	anObject class == anonClass
		ifFalse: [ anonClass adoptInstance: anObject.
			migratedObjects at: anonClass put: anObject ]
]

{ #category : #migration }
MetaLinkAnonymousClassBuilder >> migrateObjectToOriginalClass: anObject [
	| class |
	class := anObject class.
	class isAnonymous
		ifTrue: [ migratedObjects removeKey: class.
			class superclass adoptInstance: anObject ]
]

{ #category : #creation }
MetaLinkAnonymousClassBuilder >> newAnonymousSubclassFor: aClass [
	| anonSubclass |
	anonSubclass := aClass newAnonymousSubclass.
	(classes at: aClass ifAbsentPut: WeakSet new)
		add: anonSubclass.
	^ anonSubclass
]

{ #category : #creation }
MetaLinkAnonymousClassBuilder >> removeMethodNode: aNode fromObject: anObject [
	anObject class isAnonymous ifFalse:[^self].
	anObject class removeSelector: aNode methodNode selector.

]

{ #category : #accessing }
MetaLinkAnonymousClassBuilder >> soleInstanceOf: anAnonymousClass [
	^ migratedObjects at: anAnonymousClass
]
