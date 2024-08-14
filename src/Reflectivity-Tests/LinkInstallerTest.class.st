Class {
	#name : 'LinkInstallerTest',
	#superclass : 'ReflectivityTestCase',
	#instVars : [
		'obj1',
		'obj2',
		'obj3',
		'obj4'
	],
	#category : 'Reflectivity-Tests-Base',
	#package : 'Reflectivity-Tests',
	#tag : 'Base'
}

{ #category : 'running' }
LinkInstallerTest >> removeClassAccessControlForTests: anObject [
	anObject realClass removeSelector: #class
]

{ #category : 'running' }
LinkInstallerTest >> setUp [
	super setUp.
	Smalltalk garbageCollectMost.
	MetaLink uninstallAll.
	Smalltalk garbageCollectMost.
	obj1 := ReflectivityExamples new.
	obj2 := ReflectivityExamples new.
	obj3 := ReflectivityExamples new.
	obj4 := ReflectivityExamples2 new
]

{ #category : 'running' }
LinkInstallerTest >> tearDown [
	ReflectivityExamples2 new removeModifiedMethodWithInstVarAccess.
	ReflectivityExamples2 new removeNewMethodWithInstVarAccess.
	ReflectivityExamples new removeTemporaryMethods.
	super tearDown
]

{ #category : 'nodes' }
LinkInstallerTest >> testCallToSuper [
	| obj link node |
	obj := ReflectivityExamples2Subclass new.
	node := (ReflectivityExamples2Subclass  >> #methodWithOverrides) ast.
	link := MetaLink new.

	node link: link forObject: obj.

	self assert: obj methodWithOverrides equals: 'top + down'
]

{ #category : 'nodes' }
LinkInstallerTest >> testCallToSuperWithArgs [
	| obj link node |
	obj := ReflectivityExamples2Subclass new.
	node := (ReflectivityExamples2Subclass  >> #methodWithOverrides:with:) ast.
	link := MetaLink new.

	node link: link forObject: obj.

	self assert: (obj methodWithOverrides: 'top' with: 'down') equals: 'topdowntopdown'
]

{ #category : 'nodes' }
LinkInstallerTest >> testFindingAnonymousNodes [
	| link varNode instance objectSpecificVarNode |
	varNode := (ReflectivityExamples >> #exampleSendNoReturn) ast allChildren atRandom.

	link := MetaLink new.
	instance := ReflectivityExamples new.

	varNode link: link forObject: instance.
	objectSpecificVarNode := link linkInstaller linkToNodesMapper findNodeLike: varNode forObject: instance.

	self deny: varNode identicalTo: objectSpecificVarNode.
	self assert: varNode equals: objectSpecificVarNode
]

{ #category : 'nodes' }
LinkInstallerTest >> testLinkOnASTProgramNode [
	| link varNode instance objectSpecificVarNode |
	varNode := (ReflectivityExamples >> #exampleSendNoReturn) ast allChildren atRandom.
	link := MetaLink new.

	instance := ReflectivityExamples new.
	varNode link: link forObject: instance.
	objectSpecificVarNode := link linkInstaller linkToNodesMapper findNodeLike: varNode forObject: instance.

	self deny: varNode hasMetalink.
	self assert: objectSpecificVarNode hasMetalink.
	self assert: objectSpecificVarNode links asArray first identicalTo: link
]

{ #category : 'permalinks' }
LinkInstallerTest >> testLinkOnClassVar [
	| link|
	link := MetaLink new.

	link installOnVariableNamed: #classVar for: ReflectivityExamples2 option: #read instanceSpecific: false.
	self assert: link nodes size equals: 2.
	self assert: (link nodes allSatisfy: [:node| node isRead]).
	self assert: (link nodes allSatisfy: [:node| node name = #classVar]).
	link uninstall.
	self assert: link nodes isEmpty.

	link installOnVariableNamed: #classVar for: ReflectivityExamples2 option: #write instanceSpecific: false.
	self assert: link nodes size equals: 2.
	self assert: (link nodes allSatisfy: [:node| node isAssignment]).
	self assert: (link nodes allSatisfy: [:node| node variable name = #classVar]).
	link uninstall.
	self assert: link nodes isEmpty.

	link installOnVariableNamed: #classVar for: ReflectivityExamples2 option: #all instanceSpecific: false.
	self assert: link nodes size equals: 4.
	link uninstall.
	self assert: link nodes isEmpty
]

{ #category : 'permalinks' }
LinkInstallerTest >> testLinkOnClassVarForObject [
	| link obj |
	link := MetaLink new.
	obj := ReflectivityExamples2 new.

	link installOnVariableNamed: #classVar for: obj option: #read instanceSpecific: false.
	self assert: link nodes size equals: 2.
	self assert: (link nodes allSatisfy: [:node| node isRead]).
	self assert: (link nodes allSatisfy: [:node| node name = #classVar]).
	link uninstall.
	self assert: link nodes isEmpty.

	link installOnVariableNamed:  #classVar for: obj option: #write instanceSpecific: false.
	self assert: link nodes size equals: 2.
	self assert: (link nodes allSatisfy: [:node| node isAssignment]).
	self assert: (link nodes allSatisfy: [:node| node variable name = #classVar]).
	link uninstall.
	self assert: link nodes isEmpty.

	link installOnVariableNamed: #classVar for: obj option: #all instanceSpecific: false.
	self assert: link nodes size equals: 4.
	link uninstall.
	self assert: link nodes isEmpty
]

{ #category : 'permalinks' }
LinkInstallerTest >> testLinkOnTempVar [
	| methodNode link variable |
	methodNode := (ReflectivityExamples2 >> #methodWithTempVarAccess) ast.
	link := MetaLink new.
	variable := (ReflectivityExamples2 >> #methodWithTempVarAccess) lookupVar: #temp.

	link installOnVariable: variable
		for: ReflectivityExamples2
		option: #read
		instanceSpecific: false.
	self assert: link nodes size equals: 2.
	self assert: (link nodes allSatisfy: [ :node | node isRead ]).
	self assert: (link nodes allSatisfy: [ :node | node name = #temp ]).
	self assert: (link nodes allSatisfy: [ :node | node isTempVariable ]).
	link uninstall.
	self assert: link nodes isEmpty.

	variable := (ReflectivityExamples2 >> #methodWithTempVarAccess) lookupVar: #temp.
	link installOnVariable: variable
		for: ReflectivityExamples2
		option: #write
		instanceSpecific: false.
	self assert: link nodes size equals: 2.
	self assert: (link nodes allSatisfy: [ :node | node isAssignment ]).
	self assert: (link nodes allSatisfy: [ :node | node variable name = #temp ]).
	self assert: (link nodes allSatisfy: [ :node | node variable isTempVariable ]).
	link uninstall.
	self assert: link nodes isEmpty.

	variable := (ReflectivityExamples2 >> #methodWithTempVarAccess) lookupVar: #temp.
	link installOnVariable: variable
		for: ReflectivityExamples2
		option: #all
		instanceSpecific: false.
	self assert: link nodes size equals: 4.
	link uninstall.
	self assert: link nodes isEmpty
]

{ #category : 'permalinks' }
LinkInstallerTest >> testLinkOnTempVarForObject [
	|  link obj temp |

	link := MetaLink new.
	obj := ReflectivityExamples2 new.
	temp := (ReflectivityExamples2>>#methodWithTempVarAccess) lookupVar: #temp.

	link installOnVariable: temp
		for: obj
		option: #read
		instanceSpecific: false.
	self assert: link nodes size equals: 2.
	self assert: (link nodes allSatisfy: [ :node | node isRead ]).
	self assert: (link nodes allSatisfy: [ :node | node name = #temp ]).
	self assert: (link nodes allSatisfy: [ :node | node isTempVariable ]).
	link uninstall.
	self assert: link nodes isEmpty.
	temp := (ReflectivityExamples2>>#methodWithTempVarAccess) lookupVar: #temp.

	link installOnVariable: temp
		for: obj
		option: #write
		instanceSpecific: false.
	self assert: link nodes size equals: 2.
	self assert: (link nodes allSatisfy: [ :node | node isAssignment]).
	self assert: (link nodes allSatisfy: [ :node | node variable name = #temp ]).
	self assert: (link nodes allSatisfy: [ :node | node variable isTempVariable ]).
	link uninstall.
	self assert: link nodes isEmpty.

	temp := (ReflectivityExamples2>>#methodWithTempVarAccess) lookupVar: #temp.

	link installOnVariable: temp
		for: obj
		option: #all
		instanceSpecific: false.
	self assert: link nodes size equals: 4.
	link uninstall.
	self assert: link nodes isEmpty
]

{ #category : 'links - installing' }
LinkInstallerTest >> testLinkTargetsObjectsByIdentity [
	|link ast set|
	link := MetaLink new metaObject: Halt; selector: #now.
	ast := (Set >> #add:) ast.

	set := Set new.
	set link: link toAST: ast.
	self should:[set add: 1] raise: Halt.

	set := Set new.
	set link: link toAST: ast.
	self should:[set add: 1] raise: Halt
]

{ #category : 'links - installing' }
LinkInstallerTest >> testMetaLinkOnOneObject [
	"Only one metalink for on object of a particular class. Other instances of this class must remain unaffected."

	| metalink |

	"The MetaLink must change the tag value to 1 before #exampleMethod"
	metalink := MetaLink new.
	metalink selector: #value.
	metalink metaObject: [ obj1 tagExec: 1 ].
	metalink control: #before.
	(obj1 class >> #exampleMethod) ast link: metalink forObject: obj1.

	"Only the object on which has been installed is affected"
	obj1 exampleMethod.
	obj2 exampleMethod.
	self assert: obj1 tag equals: 1.
	self assert: obj2 tag equals: nil.

	"After removing the link, the object is not affected anymore"
	(obj1 class >> #exampleMethod) ast removeLink: metalink forObject: obj1.
	obj1 tagExec: nil.
	obj1 exampleMethod.
	obj2 exampleMethod.
	self assert: obj1 tag equals: nil.
	self assert: obj2 tag equals: nil
]

{ #category : 'links - installing' }
LinkInstallerTest >> testMetaLinkWithAnonymousClasses [
	| metalink |
	"We just need to link a MetaLink to an object"
	metalink := MetaLink new.
	(obj1 class >> #exampleMethod) ast link: metalink forObject: obj1.
	self removeClassAccessControlForTests: obj1.

	"One of the 2 objects must have migrated to another class"
	self assert: obj1 class ~= obj2 class.
	self assert: obj1 class superclass identicalTo: ReflectivityExamples.
	self assert: obj2 class identicalTo: ReflectivityExamples.

	"After removing the link, the object is back to its original class"
	(obj1 class >> #exampleMethod) ast removeLink: metalink forObject: obj1.
	self assert: obj1 class identicalTo: ReflectivityExamples.
	self assert: obj2 class identicalTo: ReflectivityExamples
]

{ #category : 'links - updating' }
LinkInstallerTest >> testMethodModified [
	"When modifying a method for which there was an object specific link,
	all links must be removed"

	| metalink node |
	obj1 compileTemporaryMethods.
	node := (ReflectivityExamples >> #methodToBeModified) ast allChildren atRandom.

	"Putting a link on a random node should migrate the object to an anonymous subclass"
	metalink := MetaLink new.
	node link: metalink forObject: obj1.
	self removeClassAccessControlForTests: obj1.
	self assert: obj1 class isAnonymous.

	"Modifyinh the original method should remove all object specific links and
	migrate back the object to its original class"
	ReflectivityExamples compile: 'methodToBeModified ^' , Time now printString printString.
	self deny: obj1 class isAnonymous.
	self assert: obj1 class identicalTo: ReflectivityExamples.
	self deny: metalink hasNodes.
	self assert: (metalink linkInstaller linkToNodesMapper findNodeLike: node forObject: obj1) isNil
]

{ #category : 'links - updating' }
LinkInstallerTest >> testMethodRemoved [
	"When removing a method for which there was an object specific link,
	all links must be removed"

	| metalink node |
	obj1 compileTemporaryMethods.
	node := (ReflectivityExamples >> #methodToBeRemoved) ast allChildren atRandom.

	"Putting a link on a random node should migrate the object to an anonymous subclass"
	metalink := MetaLink new.
	node link: metalink forObject: obj1.
	self removeClassAccessControlForTests: obj1.
	self assert: obj1 class isAnonymous.

	"Removing the original method should remove all object specific links and
	migrate back the object to its original class"
	ReflectivityExamples removeSelector: #methodToBeRemoved.
	self deny: obj1 class isAnonymous.
	self assert: obj1 class identicalTo: ReflectivityExamples.
	self deny: metalink hasNodes.
	self assert: (metalink linkInstaller linkToNodesMapper findNodeLike: node forObject: obj1) isNil
]

{ #category : 'permalinks' }
LinkInstallerTest >> testNodesRemovedFromLinkWhenMethodRemoved [
	"Ensures that when a method is removed and a permalink is present, the nodes are removed from the link as they do not exist anymore"

	|methodNode link|
	ReflectivityExamples2 new generateNewMethodWithInstVarAccess.
	methodNode := (ReflectivityExamples2 >> #methodWithInstVarAccess) ast.

	link := MetaLink new.
	link installOnVariableNamed: #instVar for: ReflectivityExamples2 option: #all instanceSpecific: false.
	self assert: link nodes size equals: 8.

	ReflectivityExamples2 new removeNewMethodWithInstVarAccess.
	self assert: link nodes size equals: 4.
	link uninstall
]

{ #category : 'permalinks' }
LinkInstallerTest >> testNodesRemovedFromLinkWhenMethodRemovedFromObject [
	"Ensures that when a method is removed and a permalink is present, the nodes are removed from the link as they do not exist anymore"

	|link obj|
	ReflectivityExamples2 new generateNewMethodWithInstVarAccess.
	obj := ReflectivityExamples2 new.

	link := MetaLink new.

	link installOnVariableNamed: #instVar for: obj option: #all instanceSpecific: true.

	self assert: link nodes size equals: 8.
	self assert: (link nodes allSatisfy: [:node| (obj class >> #newMethodWithInstVarAccess) ast allChildren includes: node]) .
	ReflectivityExamples2 new removeNewMethodWithInstVarAccess.
	self assert: link nodes size equals: 4.

	link uninstall.
	self assert: link nodes isEmpty
]

{ #category : 'links - installing' }
LinkInstallerTest >> testObjectsAreWeakReferenced [

	| metalink weakArray |
	metalink := MetaLink new.
	(obj1 class >> #exampleMethod) ast link: metalink forObject: obj1.
	weakArray := WeakArray with: obj1.

	self assert: weakArray first identicalTo: obj1.

	obj1 := nil.
	4 timesRepeat: [Smalltalk garbageCollect].

	self assert: weakArray first equals: nil
]

{ #category : 'links - installing' }
LinkInstallerTest >> testOneMetaLinkClassAndObjects [
	"Installs a MetaLink on one class. Installs another MetaLink on one instance of this class.
	- all instances of the class must be affected by the first MetaLink
	- only one specific instance must be affected by the second MetaLink"

	| counter metalink metalink2 |
	self flag: 'Split this test into smaller ones !'.
	counter := 0.

	"First MetaLink affects all objects"
	metalink := MetaLink new.
	metalink selector: #value.
	metalink metaObject: [counter := counter + 1].
	metalink control: #before.
	(ReflectivityExamples >> #exampleMethod) ast link: metalink.

	"The second MetaLink affects only object 3"
	metalink2 := MetaLink new.
	metalink2 selector: #value.
	metalink2 metaObject: [counter := counter + 2].
	metalink2 control: #before.
	(ReflectivityExamples >> #exampleMethod) ast link: metalink2 forObject: obj3.

	"Original method must execute as usual"
	self assert: counter equals: 0.
	obj1 exampleMethod.
	self assert: counter equals: 1.
	obj2 exampleMethod.
	self assert: counter equals: 2.

	"For object 3, the first MetaLink must be active and increase counter by 1.
	The second MetaLink must affect only this object and increase counter by 2.
	The counter value must be increased by 3."
	obj3 exampleMethod.
	self assert: counter equals: 5.

	"The link is removed from the 3 objects."
	(ReflectivityExamples >> #exampleMethod) ast removeLink: metalink.

	"Original method must execute as usual"
	obj1 exampleMethod.
	self assert: counter equals: 5.
	obj2 exampleMethod.
	self assert: counter equals: 5.

	"Object 3 still has its instance specific link"
	obj3 exampleMethod.
	self assert: counter equals: 7
]

{ #category : 'links - installing' }
LinkInstallerTest >> testOneMetaLinkManyObjects [
	| counter metalink |
	counter := 0.
	metalink := MetaLink new.
	metalink selector: #value.
	metalink metaObject: [ counter := counter + 1 ].
	metalink control: #before.
	(obj1 class >> #exampleMethod) ast link: metalink forObject: obj1.
	(obj2 class >> #exampleMethod) ast link: metalink forObject: obj2.
	(obj4 class >> #exampleMethod) ast link: metalink forObject: obj4.

	"Original method must execute as usual while the counter is increased"
	self assert: counter equals: 0.
	obj1 exampleMethod.
	self assert: counter equals: 1.
	obj2 exampleMethod.
	self assert: counter equals: 2.
	obj4 exampleMethod.
	self assert: counter equals: 3
]

{ #category : 'permalinks' }
LinkInstallerTest >> testPermaLinkNotInstalledOnObjectIfExistsInClass [

	|link obj|
	ReflectivityExamples2 new generateNewMethodWithInstVarAccess.
	obj := ReflectivityExamples2 new.

	link := MetaLink new.

	link installOnVariableNamed: #instVar for: ReflectivityExamples2 option: #all instanceSpecific: true.
	self assert: link nodes size equals: 8.

	link installOnVariableNamed: #instVar for: obj option: #all instanceSpecific: true.
	self assert: link nodes size equals: 8.

	link uninstall
]

{ #category : 'links - updating' }
LinkInstallerTest >> testPropagateClassScopedLinks [
	"When class scoped links exist on a node,
	they must also be added to all anonymous classes with copy of this node and link."

	| metalink metalink2 node anonNode |
	node := (ReflectivityExamples >> #exampleIfTrueIfFalse) ast allChildren atRandom.

	metalink := MetaLink new.
	node link: metalink.

	metalink2 := MetaLink new.
	node link: metalink2 forObject: obj1.
	self removeClassAccessControlForTests: obj1.

	anonNode := metalink linkInstaller findSubNode: node in: (obj1 class >> #exampleIfTrueIfFalse) ast.

	self assert: anonNode links size equals: 2.
	self assert: (anonNode links includes: metalink).
	
	metalink uninstall.
	metalink2 uninstall
]

{ #category : 'links - updating' }
LinkInstallerTest >> testPropagateClassScopedLinksOnMethodNode [
	"When class scoped links exist on a method node,
	they must also be added to all anonymous classes with copy of this node and link."

	| metalink metalink2 node anonNode |

	node := (ReflectivityExamples >> #exampleIfTrueIfFalse) ast.

	metalink := MetaLink new.
	node link: metalink.

	metalink2 := MetaLink new.
	node link: metalink2 forObject: obj1.

	anonNode := (obj1 class >> #exampleIfTrueIfFalse) ast.

	self assert: (anonNode links includes: metalink).
	
	metalink uninstall.
	metalink2 uninstall
]

{ #category : 'links - updating' }
LinkInstallerTest >> testPropagateClassScopedLinksRemoval [
	"When removing a class scoped link from a node,
	it must also be removed from all anonymous classes with copy of this node and link."

	| metalink metalink2 node anonNode |

	node := (ReflectivityExamples >> #exampleIfTrueIfFalse) ast allChildren atRandom.

	metalink := MetaLink new.
	node link: metalink.

	metalink2 := MetaLink new.
	node link: metalink2 forObject: obj1.

	"The link is removed from the original class"
	anonNode := metalink linkInstaller findSubNode: node in: (obj1 class >> #exampleIfTrueIfFalse) ast.
	node removeLink: metalink.

	"The link shall not be present in the anon class node"
	self deny: (anonNode links includes: metalink)
]

{ #category : 'links - updating' }
LinkInstallerTest >> testPropagateClassScopedLinksRemovalFromMethodNode [
	"When removing a class scoped link from a method node,
	it must also be removed from all anonymous classes with copy of this node and link."

	| metalink metalink2 node anonNode |

	node := (ReflectivityExamples >> #exampleIfTrueIfFalse) ast.

	metalink := MetaLink new.
	node link: metalink.

	metalink2 := MetaLink new.
	node link: metalink2 forObject: obj1.

	"The link is removed from the original class"
	anonNode := (obj1 class >> #exampleIfTrueIfFalse) ast.
	node removeLink: metalink.

	"The link shall not be present in the anon class node"
	self deny: (anonNode links includes: metalink)
]

{ #category : 'nodes' }
LinkInstallerTest >> testPropagateLinksOnRBProgramNode [
	| link link2 varNode instance objectSpecificVarNode |
	varNode := (ReflectivityExamples >> #exampleSendNoReturn) ast allChildren atRandom.
	link := MetaLink new.
	link2 := MetaLink new.
	varNode link: link.

	instance := ReflectivityExamples new.
	varNode link: link2 forObject: instance.
	objectSpecificVarNode := link2 linkInstaller linkToNodesMapper findNodeLike: varNode forObject: instance.

	self assert: varNode links size equals: 1.
	self assert: varNode links asArray first identicalTo: link.

	self assert: objectSpecificVarNode links size equals: 2.
	self assert: (objectSpecificVarNode links includes: link).
	self assert: (objectSpecificVarNode links includes: link2).
	
	
	link uninstall.
	link2 uninstall
]

{ #category : 'links - updating' }
LinkInstallerTest >> testPropagateNewClassScopedLinks [
	"When adding a class scoped link from a node,
	it must also be added to all anonymous classes with copy of this node and link."

	| metalink metalink2 node anonNode |
	node := (ReflectivityExamples >> #exampleIfTrueIfFalse) ast allChildren atRandom.

	metalink := MetaLink new.
	metalink2 := MetaLink new.
	node link: metalink2 forObject: obj1.

	"The link shall not be present in the anon class node"
	anonNode := metalink2 linkInstaller findSubNode: node in: (obj1 class >> #exampleIfTrueIfFalse) ast.
	self deny: (anonNode links includes: metalink).

	"After adding a link to the base class, it must be present on the copied node
	in the anonymous subclass"
	node link: metalink.
	self assert: (anonNode links includes: metalink).

	metalink uninstall.
	metalink2 uninstall
]

{ #category : 'links - updating' }
LinkInstallerTest >> testPropagateNewClassScopedLinksOnMethodNode [
	"When adding a class scoped link from a node,
	it must also be added to all anonymous classes with copy of this node and link."

	| metalink metalink2 node anonNode |
	node := (ReflectivityExamples >> #exampleIfTrueIfFalse) ast.

	metalink := MetaLink new.
	metalink2 := MetaLink new.
	node link: metalink2 forObject: obj1.

	"The link shall not be present in the anon class node"
	anonNode := (obj1 class >> #exampleIfTrueIfFalse) ast.
	self deny: (anonNode links includes: metalink).

	"After adding a link to the base class, it must be present on the copied node
	in the anonymous subclass"
	node link: metalink.
	self assert: (anonNode links includes: metalink).
	
	metalink uninstall.
	metalink2 uninstall
]

{ #category : 'links - removing' }
LinkInstallerTest >> testRemovingMetaLinkOnOneObject [
	"When removing a link from an object, the object looses the behavior brought by the link"

	| metalink |

	"The MetaLink must change the tag value to 1 before #exampleIfTrueIfFalse"
	metalink := MetaLink new.
	metalink selector: #value.
	metalink metaObject: [ obj1 tagExec: 1 ].
	metalink control: #before.
	(obj1 class >> #exampleIfTrueIfFalse) ast link: metalink forObject: obj1.

	"Only the object on which has been installed is affected"
	obj1 exampleIfTrueIfFalse.
	self assert: obj1 tag equals: 1.

	"After removing the link, the object is not affected anymore"
	(obj1 class >> #exampleIfTrueIfFalse) ast removeLink: metalink forObject: obj1.
	obj1 tagExec: nil.
	obj1 exampleIfTrueIfFalse.
	self assert: obj1 tag equals: nil
]

{ #category : 'links - removing' }
LinkInstallerTest >> testRemovingMethodNodes [
	| metalink metalink2 node1 node2 |

	metalink := MetaLink new.
	metalink2 := MetaLink new.

	(ReflectivityExamples >> #exampleIfTrueIfFalse) ast link: metalink forObject: obj1.
	(ReflectivityExamples >> #exampleIfTrueIfFalse) ast allChildren last link: metalink2 forObject: obj1.
	self removeClassAccessControlForTests: obj1.

	node1 := (ReflectivityExamples >> #exampleIfTrueIfFalse)ast.
	node2 := (ReflectivityExamples >> #exampleIfTrueIfFalse) ast allChildren last.

	node1 removeLink: metalink forObject: obj1.
	self assert: obj1 class isAnonymous.

	node2 removeLink: metalink2 forObject: obj1.
	self deny: obj1 class isAnonymous
]

{ #category : 'links - removing' }
LinkInstallerTest >> testRemovingNodeFromObject [
	| metalink metalink2 |

	"Two links are put on the object which is migrated to an anon subclass."
	metalink := MetaLink new.
	(obj1 class lookupSelector: #exampleMethod) ast link: metalink forObject: obj1.
	metalink2 := MetaLink new.
	(obj1 class lookupSelector: #exampleSendNoReturn) ast link: metalink2 forObject: obj1.
	self removeClassAccessControlForTests: obj1.

	"The two nodes must now exist in the anon subclass"
	self shouldnt: [ obj1 class >> #exampleSendNoReturn ] raise: KeyNotFound.
	self shouldnt: [ obj1 class >> #exampleMethod ] raise: KeyNotFound.

	"After removing metalink2, the node it was put on must be removed from the anon subclass."
	(obj1 class >> #exampleSendNoReturn) ast removeLink: metalink2 forObject: obj1.
	self should: [ obj1 class >> #exampleSendNoReturn ] raise: KeyNotFound.
	self deny: (obj1 class lookupSelector: #exampleSendNoReturn) isNil
]

{ #category : 'links - removing' }
LinkInstallerTest >> testRemovingNodes [
	| metalink metalink2 node1 node2 |

	metalink := MetaLink new.
	metalink2 := MetaLink new.

	(ReflectivityExamples >> #exampleIfTrueIfFalse) ast allChildren first link: metalink forObject: obj1.
	(ReflectivityExamples >> #exampleIfTrueIfFalse) ast allChildren fourth link: metalink2 forObject: obj1.
	self removeClassAccessControlForTests: obj1.

	node1 := (ReflectivityExamples >> #exampleIfTrueIfFalse) ast allChildren first.
	node2 := (ReflectivityExamples >> #exampleIfTrueIfFalse) ast allChildren fourth.

	node1 removeLink: metalink forObject: obj1.
	self assert: obj1 class isAnonymous.

	node2 removeLink: metalink2 forObject: obj1.
	self deny: obj1 class isAnonymous
]

{ #category : 'links - removing' }
LinkInstallerTest >> testRemovingNodesWithSuper [
	| obj link node |
	obj := ReflectivityExamples2Subclass new.
	node := (ReflectivityExamples2Subclass >> #methodWithOverrides) ast.
	link := MetaLink new.
	node link: link forObject: obj.
	self removeClassAccessControlForTests: obj.
	self assert: obj class isAnonymous.
	node removeLink: link forObject: obj.
	self deny: obj class isAnonymous
]

{ #category : 'permalinks' }
LinkInstallerTest >> testSlotOrVarLinksAddedAfterMethodAddition [

	|methodNode link|
	methodNode := (ReflectivityExamples2 >> #methodWithInstVarAccess) ast.
	
	link := MetaLink new.
	link installOnVariableNamed: #instVar for: ReflectivityExamples2 option: #all instanceSpecific: false.

	self assert: link nodes size equals: 4.

	ReflectivityExamples2 new generateNewMethodWithInstVarAccess.

	self assert: link nodes size equals: 8.
	link uninstall
]

{ #category : 'permalinks' }
LinkInstallerTest >> testSlotOrVarLinksAddedAfterMethodAdditionForObject [

	|methodNode link obj|

	methodNode := (ReflectivityExamples2 >> #methodWithInstVarAccess) ast.
	obj := ReflectivityExamples2 new.
	
	link := MetaLink new.
	link installOnVariableNamed: #instVar for: obj option: #all instanceSpecific: true.

	self assert: link nodes size equals: 4.

	ReflectivityExamples2 new generateNewMethodWithInstVarAccess.

	self assert: link nodes size equals: 8.
	link uninstall
]

{ #category : 'permalinks' }
LinkInstallerTest >> testSlotOrVarLinksRemainAfterMethodModification [
	|methodNode link|
   ReflectivityExamples2 new generateNewMethodWithInstVarAccess.
	ReflectivityExamples2 new resetModifiedMethodWithInstVarAccess.
	methodNode := (ReflectivityExamples2 >> #modifiedMethodWithInstVarAccess) ast.

	link := MetaLink new.
	link installOnVariableNamed: #instVar for: ReflectivityExamples2 option: #all instanceSpecific: false.

	self assert: link nodes size equals: 12.

	ReflectivityExamples2 new modifyMethodWithInstVarAccess.

	self assert: link nodes size equals: 10.
	self assert: (link nodes allSatisfy: [:n| n links includes: link ]).
	link uninstall
]

{ #category : 'permalinks' }
LinkInstallerTest >> testSlotOrVarLinksRemainAfterMethodModificationForObject [
	|methodNode link obj|

	obj := ReflectivityExamples2 new.
	ReflectivityExamples2 new generateNewMethodWithInstVarAccess.
	ReflectivityExamples2 new resetModifiedMethodWithInstVarAccess.
	methodNode := (ReflectivityExamples2 >> #modifiedMethodWithInstVarAccess) ast.

	link := MetaLink new.
	link installOnVariableNamed: #instVar for: obj option: #all instanceSpecific: true.

	self assert: link nodes size equals: 12.
	"self halt."
	ReflectivityExamples2 new modifyMethodWithInstVarAccess.
	self assert: link nodes size equals: 10.
	self assert: (link nodes allSatisfy: [:n| n links includes: link ]).
	link uninstall
]

{ #category : 'links - installing' }
LinkInstallerTest >> testTwoMetaLinksOneObject [
	"Two metalinks for one object of a particular class. Other instances of this class must remain unaffected."

	| metalink metalink2 |
	self flag: 'Split this test into smaller ones !'.

	"The MetaLink must change the tag value to 1 before #exampleMethod"
	metalink := MetaLink new.
	metalink selector: #value.
	metalink metaObject: [ obj1 tagExec: 1 ].
	metalink control: #before.
	(obj1 class >> #exampleMethod) ast link: metalink forObject: obj1.

	"The second MetaLink must add 1 to tag value after #exampleMethod"
	metalink2 := MetaLink new.
	metalink2 selector: #value.
	metalink2 metaObject: [ obj1 tagExec: obj1 tag + 1 ].
	metalink2 control: #before.
	(obj1 class >> #exampleMethod) ast link: metalink2 forObject: obj1.

	"Only the object on which has been installed is affected"
	obj1 tagExec: 0.
	obj1 exampleMethod.
	obj2 exampleMethod.
	self assert: obj1 tag equals: 2.
	self assert: obj2 tag equals: nil.

	"After removing the links, the object is not affected anymore.
	We should also test that one likn can be removed while the other stays active."
	(obj1 class >> #exampleMethod) ast removeLink: metalink forObject: obj1.
	(obj1 class >> #exampleMethod) ast removeLink: metalink2 forObject: obj1.
	metalink uninstall.
	metalink2 uninstall.
	obj1 tagExec: nil.
	obj1 exampleMethod.
	obj2 exampleMethod.
	self assert: obj1 tag equals: nil.
	self assert: obj2 tag equals: nil
]

{ #category : 'links - installing' }
LinkInstallerTest >> testUninstallLink [
	"A link which is object specific must be removed from every possible node when uninstalled throug the MetaLink uninstall api"

	| metalink node |
	metalink := MetaLink new.

	(ReflectivityExamples >> #exampleIfTrueIfFalse) ast link: metalink.
	(ReflectivityExamples >> #exampleIfTrueIfFalse) ast link: metalink forObject: obj1.

	node := (ReflectivityExamples >> #exampleIfTrueIfFalse) ast.
	metalink uninstall.

	self deny: obj1 class isAnonymous.
	self assert: node links isEmpty
]

{ #category : 'links - installing' }
LinkInstallerTest >> testUninstallLinkFromNode [
	"A link which is object specific must be removed from every possible node when uninstalled throug the MetaLink uninstall api"

	| metalink node |
	metalink := MetaLink new.

	(ReflectivityExamples >> #exampleIfTrueIfFalse) ast link: metalink.
	(ReflectivityExamples >> #exampleIfTrueIfFalse) ast link: metalink forObject: obj1.
	self removeClassAccessControlForTests: obj1.

	node := (ReflectivityExamples >> #exampleIfTrueIfFalse) ast.
	self assert: obj1 class isAnonymous.
	self assert: metalink nodes size equals: 2.
	self assert: (metalink nodes includes: node).
	self assert: (metalink nodes includes: ((obj1 class >> #exampleIfTrueIfFalse) ast)).

	node removeLink: metalink forObject: obj1.

	self deny: obj1 class isAnonymous.
	self assert: metalink nodes size equals: 1.
	self assert: (metalink nodes includes: node).
	
	metalink uninstall
]

{ #category : 'permalinks' }
LinkInstallerTest >> testUninstallLinkOnSlotOrVar [
	| link linkRegistry permalink |
	link := MetaLink new.
	linkRegistry := link linkInstaller linksRegistry.

	link installOnVariableNamed: #instVar
		for: ReflectivityExamples2
		option: #all
		instanceSpecific: false.

	"After installing, registry must be correct"
	permalink := (linkRegistry permaLinksFor:link) asArray first.
	self assert: (linkRegistry slotSourcesAt: ReflectivityExamples2) asArray first asArray first
		identicalTo: permalink slotOrVariable.
	self assert: (linkRegistry registeredTargetsAt: permalink slotOrVariable) asArray first
		identicalTo: permalink.

	"After uninstalling, registry must be cleared"
	link uninstall.
	self assertEmpty: link nodes.
	self assertEmpty: (linkRegistry permaLinksFor:link).
	self assertEmpty: (linkRegistry slotSourcesAt: ReflectivityExamples2) flattened.
	self assertEmpty: (linkRegistry registeredTargetsAt: permalink slotOrVariable).

	link installOnVariableNamed: #classVar
		for: ReflectivityExamples2 new
		option: #all
		instanceSpecific: false.

	"After installing, registry must be correct"
	permalink := (linkRegistry permaLinksFor:link) asArray first.
	self assert: (linkRegistry slotSourcesAt: ReflectivityExamples2) flattened first
		identicalTo: permalink slotOrVariable.
	self assert: (linkRegistry registeredTargetsAt: permalink slotOrVariable) flattened first
		identicalTo: permalink.

	"After uninstalling, registry must be cleared"
	link uninstall.
	self assertEmpty: link nodes.
	self assertEmpty: (linkRegistry permaLinksFor:link).
	self assertEmpty: (linkRegistry slotSourcesAt: ReflectivityExamples2) flattened.
	self assertEmpty: (linkRegistry registeredTargetsAt: permalink slotOrVariable)
]

{ #category : 'links - installing' }
LinkInstallerTest >> testUninstallOneMetaLinkManyObjects [
	"Globally uninstalling a link installed on many objects
		- link should not be active anymore
		- objects must recover their original classes"

	| counter metalink |
	counter := 0.

	metalink := MetaLink new.
	metalink selector: #value.
	metalink metaObject: [ counter := counter + 1 ].
	metalink control: #before.
	(obj1 class >> #exampleMethod) ast link: metalink forObject: obj1.
	(obj2 class >> #exampleMethod) ast link: metalink forObject: obj2.
	(obj4 class >> #exampleMethod) ast link: metalink forObject: obj4.

	"The link is removed from 2 out of 3 objects. It should stay active on this last object"
	(obj1 class >> #exampleMethod) ast removeLink: metalink forObject: obj1.
	(obj4 class >> #exampleMethod) ast removeLink: metalink forObject: obj4.

	"Original method must execute and only object 2 increases the counter"
	obj1 exampleMethod.
	self assert: counter equals: 0.
	obj2 exampleMethod.
	self assert: counter equals: 1.
	obj4 exampleMethod.
	self assert: counter equals: 1
]
