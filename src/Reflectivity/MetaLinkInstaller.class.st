"
#TODO and notes
- Should we replace method operations (copying, removing) by ast manipulation ?
- Peer reviews... ?
- For now, only method nodes can be affected by object specific links.

#document inst vars and their usage
anonSubclassesRegistry <Dictionary: (Class -> WeakSet of: AnonymousClass)> This table contains for a given class all its anonymous subclasses for which there are instance specific links. Each anonymous class has exactly one instance.

nodesForObjects <WeakKeyDictionary (Object -> WeakSet of: RBMethodNode)>

links <WeakKeyDictionary (MetaLink -> WeakSet of: Object)> For each instance specific link, stores all objects it does affect. Used only for counting the number of objects affected by a given link. Maybe could be removed and replaced by a computation.

#document basic usage of the link installer

#Installing
When a link is put on a specific object, an anonymous subclass of the object's class is generated and the object is migrated to this subclass. There is a 1-1 mapping between objects an their anonymous class. That means that if we put two instance specific links on two objects of class A, these objects will migrate to two different anonymous subclasses of A. However, putting a new instance specific link to one of these objects will preserve its anonymous class and will not migrate the object to another subclass.
	
If aNode is in the original class of anObject and if there are class scoped links on this node,  they are installed in the new node of the anonymous subclass. It is necessary to preserve the original instrumentation of a node which are meant to be applied for all instances of the class.

#Uninstalling
We know we can remove a node in an anonymous subclass when there are no more instance specific links on this node. There may be ""class scoped"" links remaining, but the node can be removed because all these links are present on the superclass node that was copied down in the subclass.

When there are no more nodes specific to a given object, the object is migrated back to its superclass. As there is only one anonymous subclass per object, it is expected that the anonymous subclass is garbaged and the object is now an instance of its original class.

#Linking and unlinking subtleties
As already said, when putting a link on a node for a specific object an anonymous subclass is generated  and the node is copied down from the origin class to its anonymous subclass. When adding or removing a new link to the node in the original class, we ensure that this link is also added/removed from all nodes copies in the corresponding anonymous subclasses.

#Listening for code changes
The link installer listens for method source code changes and  must update its anonymous classes nodes with those changes. Not done yet. See LinkInstaller >> #methodChanged:

Also there is the problem of renaming a method in a class for which an anonymous subclass with a copy of this method has been made.















"
Class {
	#name : #MetaLinkInstaller,
	#superclass : #Object,
	#instVars : [
		'linksRegistry',
		'anonSubclassesBuilder',
		'linkToNodesMapper',
		'superJumpLinks'
	],
	#category : #'Reflectivity-Installer'
}

{ #category : #nodes }
MetaLinkInstaller >> astFromNode: aNode forObject: anObject inClass: anonClass [
	"Getting the ast node in the anonymous class.
	If it does not exists, it is copied in the anon class from the original node."

	^ (linkToNodesMapper findNodeLike: aNode forObject: anObject)
		ifNil: [ | copyNode |
			copyNode := self copyNode: aNode inClass: anonClass.
			linkToNodesMapper addNode: copyNode forObject: anObject ]
]

{ #category : #nodes }
MetaLinkInstaller >> canRemoveMethodNodeContaining: aNode [
	| methodNode allNodes |
	methodNode := aNode methodNode.
	allNodes := methodNode allChildren select: [ :c | c hasLinks ].
	methodNode hasLinks
		ifTrue: [ allNodes add: methodNode ].
	^ allNodes allSatisfy: [ :node | (linksRegistry isNodeWithInstanceSpecificLinks: node) not ]
]

{ #category : #nodes }
MetaLinkInstaller >> collectAnonymousAstsFor: aNode [
	"Collects all ast nodes that are copies of aNode in anonymous subclasses
	of aNode class."

	| methodNode selector methodClass methods |
	methodNode := aNode methodNode.
	selector := methodNode selector.
	methodClass := methodNode methodClass.
	methods := anonSubclassesBuilder compiledMethodsOfSelector: selector inAnonSubClassesOf: methodClass.
	^ methods
		collect: [ :method | 
			| anonMethodNode |
			anonMethodNode := method ast.
			aNode isMethod
				ifTrue: [ anonMethodNode ]
				ifFalse: [ self findSubNode: aNode in: anonMethodNode ] ]
]

{ #category : #nodes }
MetaLinkInstaller >> compileMethodNodeFor: aNode in: anAnonymousClass [
	^ (anonSubclassesBuilder compileMethodFrom: aNode in: anAnonymousClass) ast
]

{ #category : #nodes }
MetaLinkInstaller >> copyNode: aNode inClass: anAnonymousClass [
	| methodNode node |
	methodNode := aNode methodNode.
	node := self compileMethodNodeFor: aNode in: anAnonymousClass.
	self linkAllFromNode: methodNode into: node.
	self installSuperJumpLinksInMethodNode: node.
	aNode = methodNode
		ifTrue: [ ^ node ].
	^ node allChildren detect: [ :n | n isEquivalentTo: aNode ]
]

{ #category : #permalinks }
MetaLinkInstaller >> findPermaLinksForSelector: selector inClass: class [
	"Permalinks in class for selector are all permalinks installed on:
	- class
	- any anonymous subclass of class implementing selector"

	| classes |
	classes := OrderedCollection with: class.
	classes addAll: (anonSubclassesBuilder allSubclassesOf: class withSelector: selector).
	^ linksRegistry permaLinksForClasses: classes
]

{ #category : #nodes }
MetaLinkInstaller >> findSubNode: node in: methodNode [
	^ methodNode allChildren detect: [ :child | child isEquivalentTo: node ] ifNone: [ nil ]
]

{ #category : #initialization }
MetaLinkInstaller >> initialize [
	anonSubclassesBuilder := MetaLinkAnonymousClassBuilder new.
	linksRegistry := MetaLinkRegistry new.
	linkToNodesMapper := MetaLinkNodesMapper new.
	superJumpLinks := OrderedCollection new.

	SystemAnnouncer uniqueInstance weak when: MetalinkChanged send: #metalinkChanged: to: self.
	SystemAnnouncer uniqueInstance weak when: MethodModified send: #methodChanged: to: self.
	SystemAnnouncer uniqueInstance weak when: MethodRemoved send: #methodRemoved: to: self.
	SystemAnnouncer uniqueInstance weak when: MethodAdded send: #methodAdded: to: self
]

{ #category : #installation }
MetaLinkInstaller >> install: aMetaLink onNode: aNode forObject: anObject [
	| anonClass ast |
	
	linksRegistry addMetaLink: aMetaLink forObject: anObject.
	anonClass := anonSubclassesBuilder anonymousClassForObject: anObject.
	ast := self astFromNode: aNode forObject: anObject inClass: anonClass.
	ast link: aMetaLink.
	anonSubclassesBuilder migrateObject: anObject toAnonymousClass: anonClass
]

{ #category : #permalinks }
MetaLinkInstaller >> installPermaLink: aPermaLink onClassVarNamed: aClassVarName [
	| classVar |	
	classVar := MetalinkTargetResolver classVariableNamed: aClassVarName for: aPermaLink targetObjectOrClass.	
	aPermaLink slotOrVariable: classVar.
	
	self registerAndInstallPermaLink: aPermaLink forTarget: classVar
]

{ #category : #permalinks }
MetaLinkInstaller >> installPermaLink: aPermaLink onSlotNamed: aSlotName [
	| slot |
	slot := MetalinkTargetResolver slotNamed: aSlotName for: aPermaLink targetObjectOrClass.
	aPermaLink slotOrVariable: slot.
	
	self registerAndInstallPermaLink: aPermaLink forTarget: slot
]

{ #category : #permalinks }
MetaLinkInstaller >> installPermaLink: aPermaLink onTempVarNamed: aTempName inMethod: aMethodName [
	| temp |
	temp := MetalinkTargetResolver lookupTemporaryNamed: aTempName inMethod: aMethodName for: aPermaLink targetObjectOrClass.

	aPermaLink slotOrVariable: temp.
	aPermaLink slotOrVarMethod: temp method.
	
	self registerAndInstallPermaLink: aPermaLink forTarget: temp
	



]

{ #category : #lookup }
MetaLinkInstaller >> installSuperJumpLinksInMethodNode: node [
	"Installs a metalink wich provokes a super jump: 
	when sending a message to super in an anonymous class, 
	instead of starting the lookup in the super class (wich is the original class of the object),
	the lookup is started in the super super class (which is the intented superclass)."

	(node allChildren select: [ :n | n isSuper ])
		do: [ :superNode | 
			| messageSendNode superSuperClass link |
			messageSendNode := superNode parent.
			superSuperClass := node methodClass superclass superclass.
			link := MetaLink new.
			link control: #instead.
			link arguments: #(arguments receiver).
			link selector: #value:value:.
			link metaObject: [ :args :receiver | receiver perform: messageSendNode selector withArguments: args inSuperclass: superSuperClass ].
			superJumpLinks add: link.
			messageSendNode link: link ]
]

{ #category : #links }
MetaLinkInstaller >> linkAllFromNode: aNode into: copyNode [
	"If aNode is the original ast node from which a copy was made - namely copyNode,
	and if this node has links, we need to add them to copyNode."
	
	aNode hasLinks
		ifTrue: [	aNode links do: [ :link | copyNode linkIfAbsent: link ] ].
		
	aNode allChildren
		do: [ :c | 
			c hasLinks
				ifTrue: [ | node |
					node := self findSubNode: c in: copyNode.
					c links do: [ :link | node linkIfAbsent: link ] ] ]
]

{ #category : #'accessing - private tests' }
MetaLinkInstaller >> linkToNodesMapper [
	^ linkToNodesMapper
]

{ #category : #'accessing - private tests' }
MetaLinkInstaller >> linksRegistry [
	^ linksRegistry 
]

{ #category : #updating }
MetaLinkInstaller >> metalinkChanged: aMetaLinkChanged [
	aMetaLinkChanged isRemove
		ifTrue: [ ^ self propagateLinkRemoval: aMetaLinkChanged link forNode: aMetaLinkChanged node ].
	aMetaLinkChanged isAdd
		ifTrue: [ self propagateLinkAddition: aMetaLinkChanged link forNode: aMetaLinkChanged node ]
]

{ #category : #updating }
MetaLinkInstaller >> methodAdded: aMethodAdded [
	self reinstallPermaLinksForMethod: aMethodAdded methodAdded
]

{ #category : #updating }
MetaLinkInstaller >> methodChanged: aMethodChanged [
	| methodNode methodClass |
	methodNode := aMethodChanged oldMethod compiledMethod ast.
	methodClass := aMethodChanged oldMethod methodClass.
	self removePermaLinksNodesReferencesFor: methodNode.
	methodClass isAnonymous
		ifTrue: [ ^ self ].	
	self removeAllAnonymousNodesIn: methodNode fromAnonSubclassesOf: methodClass.
	self reinstallPermaLinksForMethod: aMethodChanged newMethod
]

{ #category : #updating }
MetaLinkInstaller >> methodRemoved: aMethodRemoved [
	| methodNode methodClass |
	methodNode := aMethodRemoved methodRemoved compiledMethod ast.
	self removePermaLinksNodesReferencesFor: methodNode.
	methodClass := aMethodRemoved methodOrigin.
	methodClass isAnonymous
		ifTrue: [ ^ self ].
	self
		removeAllAnonymousNodesIn: methodNode
		fromAnonSubclassesOf: methodClass
]

{ #category : #permalinks }
MetaLinkInstaller >> nodesForPermaLink: permalink toBeInstalledIn: method [
	"Looking for nodes from method where to install the permalink.
	That means that each of these nodes has no links, 
	or that the link hosted by permalink is not present in their link set."
	| slotOrVar persistenceType nodes |
	slotOrVar := permalink slotOrVariable.
	persistenceType := permalink persistenceType.
	nodes := MetalinkTargetResolver slotOrVarNodesFor: slotOrVar inMethod: method option: persistenceType.
	^ nodes select: [ :node | node hasLinks not or: [ (node links includes: permalink link) not ] ]
]

{ #category : #links }
MetaLinkInstaller >> propagateLinkAddition: link forNode: aNode [
	"When:
		- a node has been copied down in anonymous subclasses
		- a link is added to this node in the original class
		> we must ensure that this link is also added in anonymous subclasses with the same node"
	(self collectAnonymousAstsFor: aNode) do: [ :ast | ast link: link ]
]

{ #category : #links }
MetaLinkInstaller >> propagateLinkRemoval: link forNode: aNode [
	"When:
		- a link has been copied down in anonymous subclasses
		- this link has to be removed in the original class
		> we must ensure that this link is also removed in anonymous subclasses with the same node"

	(self collectAnonymousAstsFor: aNode) do: [ :ast | ast removeLink: link ]
]

{ #category : #permalinks }
MetaLinkInstaller >> recursiveRemoveMethodNode: methodNode fromPermaLinks: permalink [
	"Remove nodes from the link tied to permalink.
	These nodes can be any node in methodNode's children (itself included).
	Doesn't care if the link doesn't know the node, as we have no way of knowing."
	
	permalink link nodes remove: methodNode ifAbsent: [  ].
	methodNode allChildren do: [ :node | permalink link nodes remove: node ifAbsent: [  ] ]
]

{ #category : #permalinks }
MetaLinkInstaller >> registerAndInstallPermaLink: aPermaLink forTarget: aSlotOrVar [
	| nodes |
	(linksRegistry canReinstallPermaLink: aPermaLink)
		ifFalse: [ ^ self ].
		
	linksRegistry registerPermaLink: aPermaLink.
	
	nodes := MetalinkTargetResolver resolveASTLookupFor: aSlotOrVar option: aPermaLink persistenceType.
	aPermaLink targetObjectOrClass link: aPermaLink link toNodes: nodes
]

{ #category : #permalinks }
MetaLinkInstaller >> reinstallPermaLink: permalink onNode: node [
	| link instances |
	link := permalink link.
	permalink isInstanceSpecific
		ifFalse: [ node link: link.
			^ self ].
	instances := (linksRegistry objectsForLink: link) select: [ :i | i class superclass = permalink slotOrVarClass ].
	instances do: [ :instance | node link: link forObject: instance ]
]

{ #category : #permalinks }
MetaLinkInstaller >> reinstallPermaLinksForMethod: method [
	| permalinks |
	permalinks := linksRegistry permaLinksForMethod: method.
	permalinks
		do: [ :permalink | 
			| nodes |
			nodes := self nodesForPermaLink: permalink toBeInstalledIn: method.
			nodes do: [ :node | self reinstallPermaLink: permalink onNode: node ] ]
]

{ #category : #nodes }
MetaLinkInstaller >> removeAllAnonymousNodesIn: methodNode fromAnonSubclassesOf: aClass [
	"Will check for every ast nodes in methodNode (included) if instance specific links do exist.
	If they exist, they are uninstalled."

	| selector anonClassesWithSelector |
	self flag: 'Code a bit complicated: refactoring ?'.
	selector := methodNode selector.
	anonClassesWithSelector := anonSubclassesBuilder allSubclassesOf: aClass withSelector: selector.
	anonClassesWithSelector
		do: [ :c | 
			| object anonMethodNode anonNodes |
			object := anonSubclassesBuilder soleInstanceOf: c.
			anonMethodNode := (c >> selector) ast.

			"Getting all nodes in the method with links,
			they are instance specific because located 
			in the sole instance of a anonymous class"
			anonNodes := anonMethodNode allChildren select: [ :n | n hasLinks ].

			"We remove the existing links"
			anonNodes do: [ :node | node links do: [ :link | self uninstall: link fromNode: node forObject: object ]].

			"We remove links from the anonymous method node if any"
			anonMethodNode hasLinks
				ifTrue: [ anonMethodNode links do: [ :link | self uninstall: link fromNode: anonMethodNode forObject: object ]. anonMethodNode links removeAll ] ].
	
	"Method nodes are implicetely removed from anonymous subclasses,
	and object are silently moved back to their original class
	if no instance-specific links do exist for them"
	
]

{ #category : #nodes }
MetaLinkInstaller >> removeNode: aNode forObject: anObject [
	(self canRemoveMethodNodeContaining: aNode)
		ifTrue: [ self removeSuperJumpsFor: aNode.
			anonSubclassesBuilder removeMethodNode: aNode fromObject: anObject ].
	linkToNodesMapper removeNode: aNode forObject: anObject
]

{ #category : #permalinks }
MetaLinkInstaller >> removePermaLinksNodesReferencesFor: aMethodNode [
	"Remove all permalink references of permalinks for aMethodNode (if any).
	It includes all permalink installed on any child of aMethodNode or
	any copy of aMethodNode in anonymous classes (including their children)."

	| class anonClasses selector permalinks |
	class := aMethodNode methodClass.
	selector := aMethodNode selector.
	anonClasses := anonSubclassesBuilder allSubclassesOf: class withSelector: selector.
	permalinks := self findPermaLinksForSelector: selector inClass: class.
	permalinks
		do: [ :permalink | 
			| methodNodes |
			methodNodes := IdentitySet with: aMethodNode.
			permalink isInstanceSpecific
				ifTrue: [ methodNodes addAll: (anonClasses collect: [ :anonClass | (anonClass >> selector) ast ]) ].
			methodNodes do: [ :methodNode | self recursiveRemoveMethodNode: methodNode fromPermaLinks: permalink ] ]
]

{ #category : #lookup }
MetaLinkInstaller >> removeSuperJumpsFor: methodNode [
	| nodesWithLinks |
	nodesWithLinks := methodNode allChildren select: [ :c | c hasLinks ].
	superJumpLinks
		do: [ :link | 
			(nodesWithLinks anySatisfy: [ :node | node links includes: link ])
				ifTrue: [ link uninstall ] ]
]

{ #category : #installation }
MetaLinkInstaller >> uninstall: aMetaLink fromNode: aNode forObject: anObject [
	| node |
	node := linkToNodesMapper findNodeLike: aNode forObject: anObject.
	node ifNil: [ ^ self ].
	node removeLink: aMetaLink.
	self uninstallNode: node forObject: anObject.
	aMetaLink removeNode: node
]

{ #category : #installation }
MetaLinkInstaller >> uninstallFromAllAnonymousNodes: aMetaLink [
	| objectsForLink |
	(linksRegistry isLinkObjectSpecific: aMetaLink) ifFalse:[^self].
	objectsForLink := linksRegistry objectsForLink: aMetaLink.
	objectsForLink
		do: [ :object | 
			aMetaLink nodes
				do: [ :node | 
					(linkToNodesMapper isNode: node ownedBy: object)
						ifTrue: [ self uninstall: aMetaLink fromNode: node forObject: object ] ] ]
]

{ #category : #nodes }
MetaLinkInstaller >> uninstallNode: aNode forObject: anObject [
	self removeNode: aNode forObject: anObject.
	(linkToNodesMapper findNodesForObject: anObject) 
		ifEmpty: [ anonSubclassesBuilder migrateObjectToOriginalClass: anObject ]
]

{ #category : #permalinks }
MetaLinkInstaller >> uninstallPermaLinkFor: link [
	linksRegistry unregisterPermaLinksFor: link
]

{ #category : #lookup }
MetaLinkInstaller >> uninstallSuperJumpLinks [
	superJumpLinks do: [ :link | link uninstall ]
]

{ #category : #initialization }
MetaLinkInstaller >> unsubscribeFromAnnouncers [
	SystemAnnouncer uniqueInstance unsubscribe: self
]
