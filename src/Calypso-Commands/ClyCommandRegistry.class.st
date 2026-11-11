"
I know every command declared for Calypso with the <calypsoCommands> pragma. I find the anchors (the classes whose class side has such methods), build their ClyCommandMap lazily and keep them.
For a tool context, I answer the entries of an interaction from every anchor which applies to it (see ClyToolContext>>#isDescribedByCommandAnchor:).
The default instance is reset when a declaration method or a command class changes, so new declarations are shown at once.
To see what is declared in the image, inspect:
	ClyCommandRegistry default allEntries
"
Class {
	#name : 'ClyCommandRegistry',
	#superclass : 'Object',
	#instVars : [
		'anchors',
		'maps'
	],
	#classInstVars : [
		'default'
	],
	#category : 'Calypso-Commands',
	#package : 'Calypso-Commands'
}

{ #category : 'accessing' }
ClyCommandRegistry class >> declaredAnchors [
	"The classes whose class side declares commands with the <calypsoCommands> pragma, by name."

	^ ((Pragma allNamed: ClyCommandMap pragmaName) collect: [ :pragma | pragma methodClass instanceSide ]) asIdentitySet asArray sorted: [ :first :second | first name <= second name ]
]

{ #category : 'accessing' }
ClyCommandRegistry class >> default [

	^ default ifNil: [
			  self ensureSystemSubscription.
			  default := self forAnchors: self declaredAnchors ]
]

{ #category : 'system changes' }
ClyCommandRegistry class >> ensureSystemSubscription [

	self codeChangeAnnouncer unsubscribe: self.
	self codeChangeAnnouncer weak
		when: ClassAdded , ClassRemoved , ClassModificationApplied send: #handleClassChange: to: self;
		when: MethodAdded , MethodRemoved , MethodModified send: #handleMethodChange: to: self;
		when: MethodModified send: #handleOldMethodChange: to: self
]

{ #category : 'instance creation' }
ClyCommandRegistry class >> forAnchors: anchorClasses [
	"A registry limited to some anchors, used by tests."

	^ self new setAnchors: anchorClasses
]

{ #category : 'system changes' }
ClyCommandRegistry class >> handleClassChange: aClassAnnouncement [
	"A new or removed command can change a family added with #addAllConcreteSubclassesOf:, and a removed anchor takes its commands with it."

	| changedClass |
	default ifNil: [ ^ self ].
	changedClass := aClassAnnouncement classAffected instanceSide.
	((changedClass inheritsFrom: CmCommand) or: [ default anchors includes: changedClass ]) ifTrue: [ self reset ]
]

{ #category : 'system changes' }
ClyCommandRegistry class >> handleMethodChange: aMethodAnnouncement [

	self resetIfDeclares: aMethodAnnouncement methodAffected
]

{ #category : 'system changes' }
ClyCommandRegistry class >> handleOldMethodChange: aMethodModified [
	"A method which declared commands can lose its pragma."

	self resetIfDeclares: aMethodModified oldMethod
]

{ #category : 'system changes' }
ClyCommandRegistry class >> reset [

	<script>
	self codeChangeAnnouncer unsubscribe: self.
	default := nil
]

{ #category : 'system changes' }
ClyCommandRegistry class >> resetIfDeclares: aCompiledMethod [

	default ifNil: [ ^ self ].
	(aCompiledMethod hasPragmaNamed: ClyCommandMap pragmaName) ifTrue: [ self reset ]
]

{ #category : 'accessing' }
ClyCommandRegistry >> allEntries [
	"Every command declared for Calypso, for tools and documentation."

	^ anchors flatCollect: [ :anchor | (self mapOf: anchor) allEntries ]
]

{ #category : 'accessing' }
ClyCommandRegistry >> anchors [

	^ anchors
]

{ #category : 'accessing' }
ClyCommandRegistry >> entriesOf: anInteractionName inContext: aToolContext [
	"The entries of the interaction declared on every anchor which describes the context, whether their commands can run or not."

	^ (self mapsDescribing: aToolContext) flatCollect: [ :map | map entriesOf: anInteractionName ]
]

{ #category : 'accessing' }
ClyCommandRegistry >> mapOf: anAnchorClass [

	^ maps at: anAnchorClass ifAbsentPut: [ ClyCommandMap forAnchor: anAnchorClass ]
]

{ #category : 'accessing' }
ClyCommandRegistry >> mapsDescribing: aToolContext [

	^ anchors
		  select: [ :anchor | aToolContext isDescribedByCommandAnchor: anchor ]
		  thenCollect: [ :anchor | self mapOf: anchor ]
]

{ #category : 'initialization' }
ClyCommandRegistry >> setAnchors: anchorClasses [

	anchors := anchorClasses.
	maps := IdentityDictionary new
]
