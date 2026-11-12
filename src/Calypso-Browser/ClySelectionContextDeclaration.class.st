"
I declare which context class a browser creates for its selected items. A context class declares me with a class-side method marked with the <calypsoSelectionContext> pragma:
	ClyFullBrowserMethodContext class >> selectionContextDeclaration
		<calypsoSelectionContext>
		^ ClySelectionContextDeclaration browser: ClyFullBrowserMorph itemType: ClyMethod
The context is created for the selections of the browsers which are kinds of my browser class, when the selected items are of my item type, or whatever their type when I have none (#browser:).
A browser tries the declarations of its context classes by descending priority, then by name of the context class, and creates the context of the first one which applies (#selectContextOf:for:). When none applies, it gets a ClyUnknownBrowserContext. A higher priority lets a declaration go before another one whose item type includes its own, such as ClyVariableMethodGroup, which is a kind of ClyMethodGroup.
The declarations are collected once, and collected again when a method with the pragma changes.
"
Class {
	#name : 'ClySelectionContextDeclaration',
	#superclass : 'Object',
	#instVars : [
		'browserClass',
		'itemType',
		'priority',
		'contextClass',
		'declaringMethod'
	],
	#classInstVars : [
		'default'
	],
	#category : 'Calypso-Browser-Contexts',
	#package : 'Calypso-Browser',
	#tag : 'Contexts'
}

{ #category : 'accessing' }
ClySelectionContextDeclaration class >> all [
	"The declarations of the image, in the order browsers try them."

	^ default ifNil: [
			  self ensureSystemSubscription.
			  default := self collectDeclarations ]
]

{ #category : 'instance creation' }
ClySelectionContextDeclaration class >> browser: aBrowserClass [
	"A declaration for any selection of the browser."

	^ self new setBrowserClass: aBrowserClass itemType: nil
]

{ #category : 'instance creation' }
ClySelectionContextDeclaration class >> browser: aBrowserClass itemType: anItemTypeClass [

	^ self new setBrowserClass: aBrowserClass itemType: anItemTypeClass
]

{ #category : 'private' }
ClySelectionContextDeclaration class >> collectDeclarations [

	| declarations |
	declarations := (Pragma allNamed: self pragmaName) collect: [ :pragma |
			                (pragma methodClass instanceSide perform: pragma methodSelector)
				                setContextClass: pragma methodClass instanceSide declaringMethod: pragma method;
				                yourself ].
	^ (declarations asArray , self legacyDeclarations) sort: [ :first :second |
			  first priority = second priority
				  ifTrue: [ first sortKey <= second sortKey ]
				  ifFalse: [ first priority > second priority ] ]
]

{ #category : 'system changes' }
ClySelectionContextDeclaration class >> ensureSystemSubscription [

	self codeChangeAnnouncer unsubscribe: self.
	self codeChangeAnnouncer weak
		when: ClassRemoved send: #handleClassChange: to: self;
		when: MethodAdded , MethodRemoved , MethodModified send: #handleMethodChange: to: self;
		when: MethodModified send: #handleOldMethodChange: to: self
]

{ #category : 'system changes' }
ClySelectionContextDeclaration class >> handleClassChange: aClassAnnouncement [
	"A removed context class takes its declarations with it."

	default ifNil: [ ^ self ].
	(default anySatisfy: [ :declaration | declaration contextClass == aClassAnnouncement classAffected instanceSide ]) ifTrue: [ self reset ]
]

{ #category : 'system changes' }
ClySelectionContextDeclaration class >> handleMethodChange: aMethodAnnouncement [

	self resetIfDeclares: aMethodAnnouncement methodAffected
]

{ #category : 'system changes' }
ClySelectionContextDeclaration class >> handleOldMethodChange: aMethodModified [
	"A method which declared a context can lose its pragma."

	self resetIfDeclares: aMethodModified oldMethod
]

{ #category : 'legacy' }
ClySelectionContextDeclaration class >> legacyDeclarations [
	"The class annotations which select contexts (ClyContextSelectionStrategyAnnotation), still supported by the deprecated Commander-Calypso-Compatibility package while projects move to <calypsoSelectionContext>. The annotation class is looked up by name, so Calypso works without that package."

	^ self environment
		  at: #ClyContextSelectionStrategyAnnotation
		  ifPresent: [ :annotationClass |
				  annotationClass registeredInstances asArray collect: [ :annotation |
						  ((self browser: annotation activeContext annotationUserClass itemType: annotation targetItemType) priority: annotation priority)
							  setContextClass: annotation annotatedClass declaringMethod: (annotation annotatedClass classSide lookupSelector: annotation declarationSelector);
							  yourself ] ]
		  ifAbsent: [ #(  ) ]
]

{ #category : 'accessing' }
ClySelectionContextDeclaration class >> pragmaName [
	"Class-side methods of context classes marked with this pragma answer a declaration."

	^ #calypsoSelectionContext
]

{ #category : 'system changes' }
ClySelectionContextDeclaration class >> reset [

	<script>
	self codeChangeAnnouncer unsubscribe: self.
	default := nil
]

{ #category : 'system changes' }
ClySelectionContextDeclaration class >> resetIfDeclares: aCompiledMethod [
	"Class annotations are read too, see #legacyDeclarations."

	default ifNil: [ ^ self ].
	((aCompiledMethod hasPragmaNamed: self pragmaName) or: [ aCompiledMethod hasPragmaNamed: #classAnnotation ]) ifTrue: [ self reset ]
]

{ #category : 'query' }
ClySelectionContextDeclaration class >> selectContextOf: aSelection for: aBrowser [
	"The context of the selected items of a browser, created by the first declaration which applies."

	self all do: [ :declaration |
			((declaration isForBrowser: aBrowser) and: [ declaration shouldBeAppliedFor: aSelection ]) ifTrue: [
				^ declaration createBrowserContextOf: aSelection for: aBrowser ] ].
	^ ClyUnknownBrowserContext for: aBrowser
]

{ #category : 'query' }
ClySelectionContextDeclaration class >> selectContextOfSingleItem: aDataSourceItem for: aBrowser [
	"The context of one item of a browser, to decorate its cell."

	self all do: [ :declaration |
			((declaration isForBrowser: aBrowser) and: [ declaration shouldBeAppliedFor: aDataSourceItem ]) ifTrue: [
				^ declaration createBrowserContextOfSingleItem: aDataSourceItem for: aBrowser ] ].
	^ ClyUnknownBrowserContext for: aBrowser
]

{ #category : 'accessing' }
ClySelectionContextDeclaration >> browserClass [

	^ browserClass
]

{ #category : 'accessing' }
ClySelectionContextDeclaration >> contextClass [

	^ contextClass
]

{ #category : 'context creation' }
ClySelectionContextDeclaration >> createBrowserContextOf: aSelection for: aBrowser [

	^ contextClass ofSelection: aSelection for: aBrowser
]

{ #category : 'context creation' }
ClySelectionContextDeclaration >> createBrowserContextOfSingleItem: aDataSourceItem for: aBrowser [

	^ contextClass of: { aDataSourceItem } for: aBrowser
]

{ #category : 'accessing' }
ClySelectionContextDeclaration >> declaringMethod [
	"The method marked with <calypsoSelectionContext> which answered me."

	^ declaringMethod
]

{ #category : 'testing' }
ClySelectionContextDeclaration >> isForBrowser: aBrowser [

	^ aBrowser isKindOf: browserClass
]

{ #category : 'accessing' }
ClySelectionContextDeclaration >> itemType [
	"The type of the selected items, or nil for any selection."

	^ itemType
]

{ #category : 'printing' }
ClySelectionContextDeclaration >> printOn: aStream [

	super printOn: aStream.
	aStream
		nextPut: $(;
		print: contextClass;
		nextPutAll: ' for ';
		print: browserClass.
	itemType ifNotNil: [
			aStream
				nextPutAll: ' and ';
				print: itemType ].
	aStream nextPut: $)
]

{ #category : 'accessing' }
ClySelectionContextDeclaration >> priority [

	^ priority ifNil: [ 0 ]
]

{ #category : 'accessing' }
ClySelectionContextDeclaration >> priority: aNumber [
	"Declarations of higher priority are tried first."

	priority := aNumber
]

{ #category : 'initialization' }
ClySelectionContextDeclaration >> setBrowserClass: aBrowserClass itemType: anItemTypeClass [

	browserClass := aBrowserClass.
	itemType := anItemTypeClass
]

{ #category : 'initialization' }
ClySelectionContextDeclaration >> setContextClass: aContextClass declaringMethod: aCompiledMethod [

	contextClass := aContextClass.
	declaringMethod := aCompiledMethod
]

{ #category : 'testing' }
ClySelectionContextDeclaration >> shouldBeAppliedFor: aSelectionOrItem [
	"Without item type, I apply to any selection."

	itemType ifNil: [ ^ true ].
	^ aSelectionOrItem isBasedOnItemType: itemType
]

{ #category : 'private' }
ClySelectionContextDeclaration >> sortKey [

	^ contextClass name , ' ' , declaringMethod selector
]
