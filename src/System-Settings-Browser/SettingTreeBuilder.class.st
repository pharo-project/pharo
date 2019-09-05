"
A builder for the settings tree
"
Class {
	#name : #SettingTreeBuilder,
	#superclass : #Object,
	#instVars : [
		'nodeList',
		'currentParent',
		'currentPragma'
	],
	#category : #'System-Settings-Browser'
}

{ #category : #'instance creation' }
SettingTreeBuilder class >> acceptableKeywords: aCollectionOfRegExps [ 
	^ self new acceptableKeywords: aCollectionOfRegExps 

]

{ #category : #accessing }
SettingTreeBuilder >> buildPragma: aPragma [
	currentPragma := aPragma.
	self perform: aPragma selector withArguments: aPragma arguments.
	^ nodeList
]

{ #category : #'public tree building' }
SettingTreeBuilder >> group: aSymbol [
	^ self nodeClass: PragmaSetting name: aSymbol

]

{ #category : #'public tree building' }
SettingTreeBuilder >> launcher: aSymbol [
	^ self nodeClass: SystemSettingLauncher name: aSymbol

]

{ #category : #accessing }
SettingTreeBuilder >> nodeClass [
	^ SettingNode
]

{ #category : #'private tree building' }
SettingTreeBuilder >> nodeClass: aClass name: aSymbol [
	| node |
	node := self nodeClass with: aClass new.
	node item name: aSymbol.
	node pragma: currentPragma.
	node parentName: (currentParent ifNotNil: [currentParent name]).
	self nodeList add: node.
	^ (SettingNodeBuilder new) node: node; builder: self; yourself
]

{ #category : #accessing }
SettingTreeBuilder >> nodeList [
	^ nodeList ifNil: [nodeList := OrderedCollection new]

]

{ #category : #'private tree building' }
SettingTreeBuilder >> parent: aNode while: aBlock [
	| oldParent |
	oldParent := currentParent.
	currentParent := aNode.
	aBlock value.
	currentParent := oldParent
]

{ #category : #'public tree building' }
SettingTreeBuilder >> pickOne: aSymbol [
	^ self nodeClass: PickOneSettingDeclaration name: aSymbol

]

{ #category : #'public tree building' }
SettingTreeBuilder >> range: aSymbol [
	^ self nodeClass: RangeSettingDeclaration name: aSymbol

]

{ #category : #'public tree building' }
SettingTreeBuilder >> setting: aSymbol [
	^ self nodeClass: SettingDeclaration name: aSymbol

]

{ #category : #pragmas }
SettingTreeBuilder >> systemsettings [
	"Process a <systemsettings> pragma"

	<settingPragmaProcessor>
	currentPragma methodClass instanceSide
		perform: currentPragma methodSelector
		with: self
]
