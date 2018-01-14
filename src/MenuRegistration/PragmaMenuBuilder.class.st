"
For the impatients, see MenuRegistrationExample class methods and try it with:
---------------
((PragmaMenuBuilder pragmaKeyword: MenuRegistrationExample pragmaKeyword model: nil) menuEntitled: 'World') popUpInWorld
---------------

PragmaMenuBuilder is for the dynamic building of menus based on pragmas. A PragmaMenuBuilder instance is the root of a tree of MenuRegistration instances.
The basic principle is that each menu sub-tree is specified by a method which is tagged by a specific pragma. Such methods are dynamically retrieved and then evaluated with a MenuRegistration passed as argument (see #retrieveRegistrations). The result is a tree of MenuRegistration which roots are stored in my itemList inst. var.
After the tree of MenuRegistration has been built, it is re-organized (re-organization is based on the parent declaration) and is re-ordered (based on the MenuRegistration order indications). Then the tree of MenuRegistration can serve as input for the building of a PluggableMenuSpec. The PluggableMenuSpec is itself used in order to build a MenuMorph with the help of the current ToolBuilder. (see MenuRegistration comment for more informations about how to specify menu entries).

The tree of MenuRegistration is built by #buildTree in three steps (1) the  collecting of the MenuRegistration instances (2) the re-organization and (3) the sorting:

1) The first step consists in evaluating all pragma methods by passing a builder (a PragmaMenuBuilder instance) as argument. Each pragma method invocation build a sub-tree which root is added to the builder itemList collection. (see #collectRegistrations).
As an example, this first step could produce a tree as follow (stored in a PragmaMenuBuilder itemList inst var) :
				#Tools						#'Other tool', parentName: #Tools
				/	\										|
	(#Worspace)	(#browser)						(#'Test runner' )

2) The second step consists is re-organizing the tree. A MenuRegistration can be declared with a particular parent name (by sending #parent: to it with a symbol as argument). If the parentName of a MenuRegistration X is the name of another MenuRegistration Z, then it means that X must be placed as a child of Z. This is the goal of this re-arrangement step which moves badly placed nodes at their good place. (see #arrangeRegistrations).
With previous example, the second step produces:
						#Tools								
				/		|			\
	(#Worspace)	(#browser)		#'Other tool' , parentName: #Tools
											|
									(#'Test runner')

2) The third step consists in sorting the tree according to the order inst. var. value of each MenuRegistration. This is done in two passes: the first pass tries to assign as much order inst. var. as possible (If an item is given with a specific order, then, previous and following items order can be automatically computed - see #assignOrderWithBlock: and #orderAssignBlock). The second pass consists in a smple sort according to a sort block given by #itemSortBlock.


Instance Variables
	model:		<Object>
	pragmaCollector:		<PragmaCollection>
	pragmaKeywords:		<Collection of Symbol>
	currentRoot: 			<MenuRegistration>

model
	- Serves as the default target for the menu. Note that a default target can also be declared at menu item level

pragmaKeywords
	- The list of pragma keywords used for the declaring of my menu items

pragmaCollector
	- The PragmaCollector associated with this builder. When a method declared with the same pragma as my pragmaKeyword is updated/added/removed my menu items are recomputed so that the resulting menu is always in sync with currently declared items.
	
currentRoot
	- the current MenuRegistration in which new items are to be added


"
Class {
	#name : #PragmaMenuBuilder,
	#superclass : #MenuRegistration,
	#instVars : [
		'pragmaKeywords',
		'model',
		'pragmaCollector',
		'currentRoot'
	],
	#category : #'MenuRegistration-Core'
}

{ #category : #private }
PragmaMenuBuilder class >> itemSortBlock [
	"The block which is used to sort a menu tree"
	^ [:a :b | 
		((a order notNil and: [b order notNil]) and: [a order ~= b order])
			ifTrue: [a order < b order]
			ifFalse: [((a order isNil and: [b order isNil]) or: [a order = b order])
				ifTrue: [true]
				ifFalse: [a order ifNil: [false] ifNotNil: [true]]]]
]

{ #category : #private }
PragmaMenuBuilder class >> orderAssignBlock [
	"The block which is used to set MenuRegistration tree node order inst var (which is used to sort the tree)"

	^ [ :list | 
	list
		detect: [ :n | n order notNil ]
		ifFound: [ :firstWithOrder | 
			| idx order |
			"A menu registration with an order indication hase been found - then compute order of previous and next nodes"
			idx := list indexOf: firstWithOrder.
			order := firstWithOrder order.
			idx > 1
				ifTrue: [ 
					idx - 1 to: 1 do: [ :pos | 
						(list at: pos) order: order - 1.
						order := order - 1 ] ].
			order := firstWithOrder order.
			idx + 1 to: list size do: [ :pos | 
				(list at: pos) order ifNil: [ (list at: pos) order: order + 1 ] ifNotNil: [ order := (list at: pos) order ].
				order := order + 1 ].
			list ]
		ifNone: [ list ]	"No order has been set - do not touch anything, the list order is ok" ]
]

{ #category : #'instance creation' }
PragmaMenuBuilder class >> pragmaKeyword: aPragmaKeyword model: aModel [
	"Build a builder using aPragmaKeyword as the pragma keyword and aModel a the model of the resulting builder"
	^ self withAllPragmaKeywords: (Array with: aPragmaKeyword) model: aModel
]

{ #category : #'instance creation' }
PragmaMenuBuilder class >> withAllPragmaKeywords: aCollection model: aModel [
	"Build a builder using aPragmaKeyword as the pragma keyword and aModel a the model of the resulting builder"
	^ self new 
		pragmaKeywords: aCollection;
		model: aModel;
		yourself.
]

{ #category : #'registrations handling' }
PragmaMenuBuilder >> allMisplacedItems [
	| misplaced |
	self collectMisplacedItemsIn: (misplaced := OrderedCollection new).
	^ misplaced

]

{ #category : #'registrations handling' }
PragmaMenuBuilder >> arrangeRegistrations [
	self allMisplacedItems do: [:item |
		(self itemNamed: item parentName)
			ifNotNil: [:newOwner | item owner removeItem: item.
				item owner: newOwner.
				newOwner addItem: item]].

]

{ #category : #'menu building' }
PragmaMenuBuilder >> buildTree [
	"Retrieve all menu registrations with the help of a PragmaCollector then, 
	reorganise the tree and sort it  - see class comment for more informations"
	itemList := OrderedCollection new.
	self collectRegistrations.
	self arrangeRegistrations.
	self sortRegistrations
]

{ #category : #accessing }
PragmaMenuBuilder >> builder [
	^ self
]

{ #category : #'registrations handling' }
PragmaMenuBuilder >> collectRegistrations [
	"Retrieve all pragma methods and evaluate them by passing the 
	MenuRegistration class as argument. The result is a list of trees
	stored in my itemList inst var"

	| menu |
	menu := PragmaMenuAndShortcutRegistration model: self model.
	self pragmaCollector
		do: [ :prg | 
			self
				currentRoot: self
				while: [ prg methodClass theNonMetaClass
						perform: prg methodSelector
						with: menu ] ].
	self interpretRegistration: menu
]

{ #category : #'menu building' }
PragmaMenuBuilder >> currentRoot: anItem while: aBlock [
	| old |
	old := currentRoot.
	currentRoot := anItem.
	[aBlock value] ensure: [currentRoot := old]
]

{ #category : #initialization }
PragmaMenuBuilder >> initialize [
	super initialize.
	isGroup := true.
	currentRoot := self.
	pragmaKeywords := OrderedCollection new.
	
]

{ #category : #private }
PragmaMenuBuilder >> interpretRegistration: aRegistration [ 
	| root |
	root := MenuRegistration owner: self.
	aRegistration handOutItems do: [:item || node |
			node := item group
					ifNil: [ root item: item item ]
					ifNotNil: [:grp | root group: grp ].
			item with
				ifNil: [
					item action
						ifNil: [ 
							node 
								target: item target;
								arguments: item arguments;
								selector: item selector ]
						ifNotNil:[ 
							node 
								action: item action ]].
			node
				keyText: item keyText;
				help: item help;
				icon: item icon;
				order: item order;
				parent: item parent.
			item enabled
				ifNil: [ node enabledBlock: item enabledBlock ]
				ifNotNil: [:boolean | node enabled: boolean ].	
			item label
				ifNotNil: [ node label: item label ].
			item default ifNotNil: [ :d |
				node label: node label, ' (', d asString, ')'].
			item isWithSeparatorAfter
				ifTrue: [ node withSeparatorAfter ]]
]

{ #category : #accessing }
PragmaMenuBuilder >> itemReceiver [
	^ model
]

{ #category : #'public menu building' }
PragmaMenuBuilder >> menuSpec [
	"returns a PluggableMenuSpec build from my contents"
	^ self menuSpecAt: nil
]

{ #category : #'public menu building' }
PragmaMenuBuilder >> menuSpecAt: aName [
	"returns a PluggableMenuSpec build from my contents starting at 
	the inner MenuRegistration named aName or from here if aName is nil"
	| root |
	self buildTree.
	root := PluggableMenuSpec withModel: nil.
	(aName ifNil: [self] ifNotNil: [self itemNamed: aName]) 
		ifNotNil: [:top | top precondition value ifTrue: [top buildMenuSpec: root]].
	^ root
]

{ #category : #accessing }
PragmaMenuBuilder >> model [
	^ model
]

{ #category : #accessing }
PragmaMenuBuilder >> model: anObject [
	model := anObject
]

{ #category : #'menu building' }
PragmaMenuBuilder >> newSubItem [
	| reg |
	reg := MenuRegistration owner: currentRoot.
	currentRoot addItem: reg.
	^ reg
]

{ #category : #'registrations handling' }
PragmaMenuBuilder >> pragmaCollector [
	"Return an up-to-date pragmaCollector which contains all pragmas which keyword is self pragmaKeyword"

	^ pragmaCollector
		ifNil: [ pragmaCollector := PragmaCollector
				filter: [ :prg | 
					(self pragmaKeywords includes: prg keyword)
						and: [ prg methodSelector numArgs = 1 ] ].
			(self pragmaKeywords notNil and: [ self pragmaKeywords notEmpty ])
				ifTrue: [ pragmaCollector reset ].
			pragmaCollector whenChangedSend: #reset to: self.
			pragmaCollector ]
]

{ #category : #accessing }
PragmaMenuBuilder >> pragmaKeyword [
	"Deprecated"
	^  pragmaKeywords isEmptyOrNil ifFalse: [pragmaKeywords first]
]

{ #category : #accessing }
PragmaMenuBuilder >> pragmaKeyword: aString [
	"Set the pragma keyword used to select pragmas (see #pragmaCollector)"
	pragmaKeywords add: aString asSymbol.
	self pragmaCollector reset.
]

{ #category : #accessing }
PragmaMenuBuilder >> pragmaKeywords [
	"Returns the pragma keyword used to select pragmas (see #pragmaCollector)"
	^  pragmaKeywords 
]

{ #category : #accessing }
PragmaMenuBuilder >> pragmaKeywords: aCollection [
	"Returns the pragma keyword used to select pragmas (see #pragmaCollector)"
	pragmaKeywords addAll: (aCollection collect: [:k | k asSymbol])
]

{ #category : #'initialize-release' }
PragmaMenuBuilder >> release [
	self pragmaCollector unsubscribe: self.
	pragmaCollector := nil.
	model := nil.
	super release
	
]

{ #category : #'registrations handling' }
PragmaMenuBuilder >> sortRegistrations [
	"Try to update MenuRegistration order inst. var and the sort the trees"
	self assignOrderWithBlock: self class orderAssignBlock.
	self sort: self class itemSortBlock
]
