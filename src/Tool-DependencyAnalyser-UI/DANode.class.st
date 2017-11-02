"
A PDNode is an abstract class for to display nodes in a tree.
"
Class {
	#name : #DANode,
	#superclass : #TreeNodePresenter,
	#instVars : [
		'treeModelParent',
		'packageTreeNautilus',
		'packageTreeNautilusUI',
		'cycle'
	],
	#category : 'Tool-DependencyAnalyser-UI-Nodes'
}

{ #category : #building-menu }
DANode >> addMenuEntriesOn: aMenuGroup [
	"overide this method to add contextual menu on nodes"
]

{ #category : #browse-nautilus }
DANode >> browseClass: classToBrowse [
	Smalltalk tools browser openOnClass: classToBrowse
]

{ #category : #browse-nautilus }
DANode >> browseClassInNautilus: aClass [
	| nautilus |
	
	nautilus := self rootNode packageTreeNautilus.
	self setSelectionOnMethod: nil class: aClass with: nautilus.
	self refreshNautilus: nautilus.
	self rootNode packageTreeNautilusUI sourceTextModel announce: NautilusClassSelected.
]

{ #category : #browse-nautilus }
DANode >> browseInNautilus [
	^ self subclassResponsibility
]

{ #category : #browse-nautilus }
DANode >> browseInOtherNautilus [
	"do nothing by default"
]

{ #category : #browse-nautilus }
DANode >> browseMethodInNautilus: aMethod [
	| classToBrowse nautilus compiledMethod nautilusUI |
	nautilus := self rootNode packageTreeNautilus.
	nautilusUI := self rootNode packageTreeNautilusUI.
	classToBrowse := aMethod methodClass.
	compiledMethod := classToBrowse>>aMethod selector.
	self setSelectionOnMethod: compiledMethod class: classToBrowse with: nautilus.
	nautilusUI updateCategoryAndMethodList.
	nautilusUI sourceTextModel announce: NautilusMethodSelected.	
	
	
]

{ #category : #browse-nautilus }
DANode >> browsePackageInNautilus: aPackage [
	| classToBrowse classFromPackage classesToBrowse |
	classFromPackage := aPackage classes.
	classesToBrowse := classFromPackage reject: [ :class | classFromPackage includes: class superclass ].
	classToBrowse := classesToBrowse isEmpty
		ifFalse: [ (classesToBrowse sorted: [ :a :b | a asString < b asString ]) first ].
	classToBrowse ifNotNil: [ self browseClassInNautilus: classToBrowse ]
]

{ #category : #accessing }
DANode >> cycle: anObject [
	cycle := anObject
]

{ #category : #testing }
DANode >> isClassToDependencyNode [
	^ false
]

{ #category : #testing }
DANode >> isCycle [
	^ cycle
]

{ #category : #testing }
DANode >> isCycleNode [
	^ false
]

{ #category : #testing }
DANode >> isPackageNode [
 	^ false
]

{ #category : #testing }
DANode >> isReferenceNode [
	^ false
]

{ #category : #displaying }
DANode >> label [
	^ self content displayString
]

{ #category : #accessing }
DANode >> packageTreeNautilus [
	^ packageTreeNautilus
]

{ #category : #accessing }
DANode >> packageTreeNautilus: anObject [
	packageTreeNautilus := anObject
]

{ #category : #accessing }
DANode >> packageTreeNautilusUI [
	^ packageTreeNautilusUI
]

{ #category : #accessing }
DANode >> packageTreeNautilusUI: anObject [
	packageTreeNautilusUI := anObject
]

{ #category : #accessing }
DANode >> packageUnderAnalysis [
	^ self rootNode daPackage rPackageSet packages first
]

{ #category : #browse-nautilus }
DANode >> refreshNautilus: nautilus [

	| nautilusUI |
	nautilusUI := self rootNode packageTreeNautilusUI.
	nautilusUI updatePackageGroupAndClassList.
]

{ #category : #private }
DANode >> rootNode [
	| rootNode currentNode |
	currentNode := self.
	rootNode := currentNode parentNode.
	[ rootNode isNil ]
		whileFalse: [ 
			currentNode := rootNode.
			rootNode := currentNode parentNode ].
	^ currentNode.
]

{ #category : #browse-nautilus }
DANode >> setSelectionOnMethod: aMethod class: aClass with: nautilus [
	| protocol nautilusUI |
	aMethod ifNotNil: [ protocol := aMethod protocol ].
	nautilusUI := self rootNode packageTreeNautilusUI.
	self refreshNautilus: nautilus.
	nautilus
		browsedEnvironment: Nautilus defaultBrowsedEnvironment;
		showInstance: aClass isMeta not.
	nautilus
		package: aClass package
		class: aClass
		protocol: protocol
		method: aMethod.
	nautilusUI
		showHierarchy: false;
		giveFocusToMethod
]

{ #category : #accessing }
DANode >> treeModelParent [
	^ treeModelParent
]

{ #category : #accessing }
DANode >> treeModelParent: anObject [
	treeModelParent := anObject
]
