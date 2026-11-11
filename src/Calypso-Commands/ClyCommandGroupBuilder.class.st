"
I build the Commander2 command groups (CmCommandGroup) that Calypso tools show.
- #buildFor:context: and #buildForAll:context: instantiate every concrete subclass of base command classes with a tool context and keep the commands which can run in it.
- #buildMenuOf:context:pureRoots: and #buildToolbarOf:contexts: build the trees of menus, toolbars and tab actions (ClyPresentedGroup and ClyPresentedCommand) from activations. The activations are class annotations (ClyCommandActivationStrategy) or entries declared with the <calypsoCommands> pragma (ClyCommandEntry, see ClyCommandRegistry).
- #buildMenuFor:context:pureRoots: and #buildToolbarFor:contexts: do the same with the class annotations of an activation strategy class.
Use ClyCommandGroupMenuRenderer to display the trees in Morphic.
"
Class {
	#name : 'ClyCommandGroupBuilder',
	#superclass : 'Object',
	#category : 'Calypso-Commands',
	#package : 'Calypso-Commands'
}

{ #category : 'instance creation' }
ClyCommandGroupBuilder class >> buildFor: aCommandClass context: aContext [

	^ self new
		  buildFor: aCommandClass
		  context: aContext
]

{ #category : 'instance creation' }
ClyCommandGroupBuilder class >> buildSortedForAll: commandClasses context: aContext [

	^ self new buildForAll: commandClasses context: aContext
]

{ #category : 'building' }
ClyCommandGroupBuilder >> buildFor: aCommandClass context: aContext [

	| commands group |
	commands := OrderedCollection new.
	aCommandClass allSubclasses do: [ :aClass |
			| command |
			aClass hasAbstractMethods ifFalse: [
					command := aClass new
						           context: aContext;
						           yourself.
					command canBeExecuted ifTrue: [ commands add: command ] ] ].
	group := CmCommandGroup named: aCommandClass name asString.
	commands do: [ :comm | group register: comm ].
	^ group
]

{ #category : 'building' }
ClyCommandGroupBuilder >> buildForAll: commandClasses context: aContext [

	| classes seen commands group |
	classes := OrderedCollection new.
	seen := Set new.
	commandClasses do: [ :root |
			root allSubclasses do: [ :aClass |
					(seen includes: aClass) ifFalse: [
							seen add: aClass.
							classes add: aClass ] ] ].
	commands := OrderedCollection new.
	classes do: [ :aClass |
			(aClass hasAbstractMethods or: [ (aClass includesSelector: #order) not ]) ifFalse: [
					| command |
					command := aClass new
						           context: aContext;
						           yourself.
					command canBeExecuted ifTrue: [ commands add: command ] ] ].
	commands := self sortedByOrder: commands.
	group := CmCommandGroup named: 'Method'.
	commands do: [ :command | group register: command ].
	^ group
]

{ #category : 'building' }
ClyCommandGroupBuilder >> buildMenuFor: strategyClass context: aToolContext pureRoots: rootClasses [
	"Answers the Commander2 tree of the menu of aToolContext from the activations of strategyClass visible in it, then the pure commands of rootClasses."

	| activations |
	activations := OrderedCollection new.
	strategyClass visibleInstancesInContext: aToolContext do: [ :activation | activations add: activation ].
	^ self buildMenuOf: activations context: aToolContext pureRoots: rootClasses
]

{ #category : 'building' }
ClyCommandGroupBuilder >> buildMenuOf: activations context: aToolContext pureRoots: rootClasses [
	"Answers the Commander2 tree of the menu of aToolContext: the presentation that ClyMenu gives. Hybrid commands come from the activations, which are class annotations or ClyCommandEntry instances visible in the context, then the pure commands of rootClasses are placed in their groups, in the same order consumers add them. Each group keeps its entries sorted as ClyMenuGroup does. Identifiers are unique inside their group; labels may repeat."

	| root groups entries names sortBlock uniqueName ensureGroup |
	sortBlock := [ :a :b |
		             a order = b order
			             ifTrue: [ a dynamicName <= b dynamicName ]
			             ifFalse: [ a order <= b order ] ].
	root := ClyPresentedGroup new.
	root descriptor: ClyRootMenuGroup new.
	root name: ClyRootMenuGroup name.
	groups := IdentityDictionary new.
	entries := IdentityDictionary new.
	names := IdentityDictionary new.
	groups at: ClyRootMenuGroup put: root.
	entries at: root put: (SortedCollection sortBlock: sortBlock).
	names at: root put: Set new.
	uniqueName := [ :group :base |
		              | candidate count |
		              count := 1.
		              candidate := base.
		              [ (names at: group) includes: candidate ] whileTrue: [
				              count := count + 1.
				              candidate := base , '#' , count printString ].
		              (names at: group) add: candidate.
		              candidate ].
	ensureGroup := nil.
	ensureGroup := [ :groupClass |
		               groups at: groupClass ifAbsent: [
				               | parent group |
				               parent := ensureGroup value: groupClass parentGroup.
				               group := ClyPresentedGroup new.
				               group descriptor: groupClass new.
				               group name: (uniqueName value: parent value: groupClass name).
				               groups at: groupClass put: group.
				               entries at: group put: (SortedCollection sortBlock: sortBlock).
				               names at: group put: Set new.
				               (entries at: parent) add: group.
				               group ] ].
	activations do: [ :activation |
			| activator group item |
			activator := activation newActivatorFor: aToolContext.
			group := ensureGroup value: activation menuGroup.
			item := ClyPresentedCommand new.
			item decorate: activator command.
			item activator: activator.
			item identifier: (uniqueName value: group value: activator command class name).
			(entries at: group) add: item ].
	(ClyCommandGroupBuilder buildSortedForAll: rootClasses context: aToolContext) commands do: [ :command |
			| group item |
			group := ensureGroup value: command menuGroup.
			item := ClyPresentedCommand new.
			item decorate: command.
			item identifier: (uniqueName value: group value: command class name).
			(entries at: group) add: item ].
	groups valuesDo: [ :group | (entries at: group) do: [ :each | group register: each ] ].
	^ root
]

{ #category : 'building' }
ClyCommandGroupBuilder >> buildToolbarFor: strategyClass contexts: toolContexts [
	"Answers the Commander2 tree of a toolbar or of the browser tab actions from the activations of strategyClass visible in each context."

	^ self
		  buildToolbarOf: [ :toolContext |
				  | activations |
				  activations := OrderedCollection new.
				  strategyClass visibleInstancesInContext: toolContext do: [ :activation | activations add: activation ].
				  activations ]
		  contexts: toolContexts
]

{ #category : 'building' }
ClyCommandGroupBuilder >> buildToolbarOf: activationsBlock contexts: toolContexts [
	"Answers the Commander2 tree of a toolbar or of the browser tab actions: one presented group per toolbar group class, holding the presented activations that activationsBlock answers for every context. The activations are class annotations or ClyCommandEntry instances. Groups and entries are sorted as ClyToolbarBuilder sorts them."

	| root groups entries names sortBlock uniqueName ensureGroup |
	sortBlock := [ :a :b |
		             a order = b order
			             ifTrue: [ a dynamicName <= b dynamicName ]
			             ifFalse: [ a order <= b order ] ].
	root := ClyPresentedGroup new.
	root descriptor: ClyToolbarGroup new.
	root name: ClyToolbarGroup name.
	groups := IdentityDictionary new.
	entries := IdentityDictionary new.
	names := IdentityDictionary new.
	entries at: root put: (SortedCollection sortBlock: sortBlock).
	names at: root put: Set new.
	uniqueName := [ :group :base |
		              | candidate count |
		              count := 1.
		              candidate := base.
		              [ (names at: group) includes: candidate ] whileTrue: [
				              count := count + 1.
				              candidate := base , '#' , count printString ].
		              (names at: group) add: candidate.
		              candidate ].
	ensureGroup := [ :groupClass |
		               groups at: groupClass ifAbsent: [
				               | group |
				               group := ClyPresentedGroup new.
				               group descriptor: groupClass new.
				               group name: (uniqueName value: root value: groupClass name).
				               groups at: groupClass put: group.
				               entries at: group put: (SortedCollection sortBlock: sortBlock).
				               names at: group put: Set new.
				               (entries at: root) add: group.
				               group ] ].
	toolContexts do: [ :toolContext |
			(activationsBlock value: toolContext) do: [ :activation |
					| activator group item |
					activator := activation newActivatorFor: toolContext.
					group := ensureGroup value: activation menuGroup.
					item := ClyPresentedCommand new.
					item decorate: activator command.
					item activator: activator.
					item identifier: (uniqueName value: group value: activator command class name).
					(entries at: group) add: item ] ].
	(entries at: root) do: [ :each | root register: each ].
	groups valuesDo: [ :group | (entries at: group) do: [ :each | group register: each ] ].
	^ root
]

{ #category : 'private' }
ClyCommandGroupBuilder >> sortedByOrder: commands [
	"Compatibility method that should be removed in the future"

	| index |
	index := 0.
	^ ((commands collect: [ :command |
			    index := index + 1.
			    command -> index ]) asSortedCollection: [ :a :b | a key order < b key order or: [ a key order = b key order and: [ a value <= b value ] ] ]) collect: [ :association | association key ]
]
