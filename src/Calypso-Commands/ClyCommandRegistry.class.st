"
I know every command declared for Calypso with the <calypsoCommands> pragma. I find the anchors (the classes whose class side has such methods), build their ClyCommandMap lazily and keep them.
Calypso asks me the commands of an interaction for a tool context: #activationsOf:inContext: answers the entries declared on every anchor which describes the context (see ClyToolContext>>#isDescribedByCommandAnchor:), sorted by priority. Menus, toolbars, tab actions, table icons, shortcuts, clicks and drag and drop are all built from it.
Class annotations of commands which are not migrated yet are still shown: #legacyAnnotationsOf: adds them to the activations. Code that still enumerates the annotation strategies directly also sees the declared entries, see ClyCommandActivationStrategy class>>#registeredInstances.
The default instance is reset when a declaration method or a command class changes, so new declarations are shown at once.
To see what is declared in the image, inspect:
	ClyCommandRegistry default allEntries
"
Class {
	#name : 'ClyCommandRegistry',
	#superclass : 'Object',
	#instVars : [
		'anchors',
		'maps',
		'entriesByInteraction'
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

{ #category : 'activations' }
ClyCommandRegistry >> activationsOf: anInteractionName inContext: aToolContext [
	"The entries of the interaction declared for the context, and the class annotations of the same interaction which describe it, by priority. Calypso shows them in this order, and runs the first active one for a key, a click or a drop. The annotations come first among the activations of the same priority, as before."

	| activations |
	activations := ((self legacyAnnotationsOf: anInteractionName) select: [ :annotation | annotation isVisibleInContext: aToolContext ]) asArray
	               , (self entriesOf: anInteractionName inContext: aToolContext) asArray.
	^ activations sort: (self sortBlockOf: anInteractionName)
]

{ #category : 'activations' }
ClyCommandRegistry >> activatorsOf: anInteractionName inContext: aToolContext [
	"The activators of the commands of the interaction which can run in the context."

	^ (self activeActivationsOf: anInteractionName inContext: aToolContext) collect: [ :activation | activation newActivatorFor: aToolContext ]
]

{ #category : 'activations' }
ClyCommandRegistry >> activeActivationsOf: anInteractionName inContext: aToolContext [

	^ (self activationsOf: anInteractionName inContext: aToolContext) select: [ :activation | activation isActiveInContext: aToolContext ]
]

{ #category : 'accessing' }
ClyCommandRegistry >> allEntries [
	"Every command declared for Calypso, for tools and documentation."

	^ anchors flatCollect: [ :anchor | (self mapOf: anchor) allEntries ]
]

{ #category : 'accessing' }
ClyCommandRegistry >> allEntriesOf: anInteractionName [
	"The entries of an interaction declared on every anchor, whatever the context."

	^ entriesByInteraction at: anInteractionName ifAbsentPut: [ anchors flatCollect: [ :anchor | (self mapOf: anchor) entriesOf: anInteractionName ] ]
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

{ #category : 'activations' }
ClyCommandRegistry >> executeFirstCommandOf: anInteractionName inContext: aToolContext [
	"Runs the first command of the interaction which can run in the context, for a click for example. Answers whether one ran."

	(self activeActivationsOf: anInteractionName inContext: aToolContext) ifNotEmpty: [ :activations |
			(activations first newActivatorFor: aToolContext) executeCommand.
			^ true ].
	^ false
]

{ #category : 'legacy' }
ClyCommandRegistry >> legacyAnnotationsOf: anInteractionName [
	"The class annotations of Calypso commands for the interaction, which are still supported while projects move to <calypsoCommands> declarations. See ClyCommandActivationStrategy class>>#interactionName."

	^ (ClyCommandActivationStrategy allSubclasses select: [ :strategy | strategy interactionName = anInteractionName ]) flatCollect: [ :strategy | strategy registry allInstancesOf: strategy ]
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
	maps := IdentityDictionary new.
	entriesByInteraction := IdentityDictionary new
]

{ #category : 'activations' }
ClyCommandRegistry >> shortcutsOf: aCommandClass inContext: aToolContext [
	"The shortcut activations of a command which can run in the context, to show their keys in menus."

	^ (self activeActivationsOf: #shortcuts inContext: aToolContext) select: [ :shortcut | shortcut annotates: aCommandClass ]
]

{ #category : 'activations' }
ClyCommandRegistry >> shortcutsOpeningGroup: aMenuGroupClass inContext: aToolContext [
	"The shortcuts which open the commands of the menu group in the context, to show their keys next to its sub-menu."

	^ (self shortcutsOf: ClyOpenMenuGroupCommand inContext: aToolContext) select: [ :shortcut | shortcut openedGroup = aMenuGroupClass ]
]

{ #category : 'private' }
ClyCommandRegistry >> sortBlockOf: anInteractionName [
	"Activations run by priority, the highest first. Table icons are the opposite: the command of the highest priority decorates the cell last, so its icons are the ones shown."

	anInteractionName = #tableIcons ifTrue: [ ^ [ :first :second | first priority <= second priority ] ].
	^ [ :first :second | first priority >= second priority ]
]
