"
I am a meta-link that is transforms a reference to a deprecated class to its replacement for subsequent calls to the AST's method.
"
Class {
	#name : #ClassDeprecationMetaLink,
	#superclass : #MetaLink,
	#classInstVars : [
		'enabled'
	],
	#category : #ClassDeprecation
}

{ #category : #protocol }
ClassDeprecationMetaLink class >> disable [
	"Disable deprecation transformations if not already disabled. Uninstall all installed meta-links."

	<script>
	enabled
		ifFalse: [ ^ self ].
	enabled := false.
	SystemAnnouncer uniqueInstance unsubscribe: self.
	self allInstancesDo: #uninstall
]

{ #category : #protocol }
ClassDeprecationMetaLink class >> enable [
	"Enables deprecation transformations if not already enabled. Installs all meta-links required to perform transformations on execution on already deprecated transformable classes."

	<script>
	enabled
		ifTrue: [ ^ self ].
	enabled := true.
	SystemAnnouncer uniqueInstance weak
		when: MethodAdded , MethodModified
			send: #onMethodAddedOrModified:
			to: self;
		when: MethodRemoved send: #onMethodRemoved: to: self.
	self environment allClassesAndTraits
		select: #isDeprecatedTransformable
		thenDo: #installDeprecationTransformMetaLinks
]

{ #category : #'brick-interactable-actions' }
ClassDeprecationMetaLink class >> enable: aBoolean [
	aBoolean
		ifTrue: [ self enable ]
		ifFalse: [ self disable ]
]

{ #category : #'brick-interactable-actions' }
ClassDeprecationMetaLink class >> enabled [
	^ enabled
]

{ #category : #'class initialization' }
ClassDeprecationMetaLink class >> initialize [
	enabled := false
]

{ #category : #protocol }
ClassDeprecationMetaLink class >> onMethodAddedOrModified: anEvent [
	"When a method is added or modified, we have to handle two cases:

i) The method is the #isDeprecationTransformableMethod. In that case we have to install meta-links for the transformation of existing references to the deprecated class.
ii) The method #hasDeprecationTransformableReferences and is not #isDeprecated. In that case we install meta-links for references to deprecated transformable classes"

	| method |
	method := anEvent method.
	method isDeprecationTransformableMethod
		ifTrue: [ method methodClass instanceSide isDeprecated
				ifTrue:
					[ method methodClass instanceSide installDeprecationTransformMetaLinks ] ]
		ifFalse: [ (method hasDeprecationTransformableReferences
				and: [ method isDeprecated not ])
				ifTrue: [ method installDeprecationTransformMetaLinks ] ]
]

{ #category : #protocol }
ClassDeprecationMetaLink class >> onMethodRemoved: anEvent [
	"If the removed method is the #isDeprecationTransformableMethod, we remove all existing meta-link for deprecation transforms, as they are no longer valid."

	| method |
	method := anEvent method.
	method isDeprecationTransformableMethod
		ifTrue: [ method methodClass instanceSide
				uninstallDeprecationTransformMetaLinks ]
]

{ #category : #settings }
ClassDeprecationMetaLink class >> settingsOn: aBuilder [
	<systemsettings>
	(aBuilder setting: #classDeprecation)
		label: 'Automated class deprecation transformation' translated;
		description:
			'If true, then references to classes that are deprecated and have a #transformTo: pragma will be automatically migrated using the replacement expression'
				translated;
		setSelector: #enable:;
		getSelector: #enabled;
		target: self;
		parent: #tools
]

{ #category : #'as yet unclassified' }
ClassDeprecationMetaLink >> deprecationTransform: aNode [
	"Transforms the deprecated node with its replacement and compiles the method to materialize the changes."

	aNode deprecationTransform.
	Author
		useAuthor: 'AutoDeprecationRefactoring'
		during: [ | methodNode method |
			methodNode := aNode methodNode.
			method := methodNode method.
			method origin
				compile: methodNode formattedCode
				classified: method protocol ]
]

{ #category : #initialization }
ClassDeprecationMetaLink >> initialize [
	super initialize.
	self
		metaObject: self;
		control: #before;
		selector: #deprecationTransform:;
		arguments: #(node).
]
