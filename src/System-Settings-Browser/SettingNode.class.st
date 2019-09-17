"
A SettingTreeNode is used by a SettingCollector as a node for setting trees. Built trees can be viewed by a SettingBrowser

Instance Variables
	contents:		<Collection of SettingTreeNode>
	parentNode:		<SettingTreeNode or nil>

contents
	- the children

parentNode
	- the parent or nil if the node is a root

"
Class {
	#name : #SettingNode,
	#superclass : #MorphTreeNodeModel,
	#instVars : [
		'parentName',
		'pragma',
		'contents'
	],
	#category : #'System-Settings-Browser'
}

{ #category : #comparing }
SettingNode >> <= other [
	^ self asString <= other asString
]

{ #category : #accessing }
SettingNode >> allChildren [
	^ self model childrenOf: self

]

{ #category : #menu }
SettingNode >> browseDeclaration [
	"open a browser on the method which defines myself"
	Smalltalk tools browser
		openOnClass: self methodClass class selector: self selector
]

{ #category : #checking }
SettingNode >> checkForUnknownTarget [
	self parentNode ifNotNil: [:p | item target ifNil: [item target: p item target].
		item targetSelector ifNil: [item targetSelector: p item targetSelector]].
	self allChildren do: [:c | c checkForUnknownTarget]
]

{ #category : #accessing }
SettingNode >> contents [
	(self settingDeclaration hasValue
			and: [self settingValue = false])
		ifTrue: [^ OrderedCollection new]
		ifFalse: [^ contents 
				ifNil: [contents := self allChildren.
					self settingDeclaration ordering
						ifTrue: [contents := contents asArray sort: self model sortBlock].
					contents := self model retainedNodesFromList: contents asOrderedCollection]]
]

{ #category : #menu }
SettingNode >> deeplyDetect: aBlock [ 
	(aBlock value: self) ifTrue: [^ self].
	self contents do: [:sub | (sub deeplyDetect: aBlock) ifNotNil: [:found | ^ found]].
	^ nil
]

{ #category : #menu }
SettingNode >> deeplySelect: aBlock in: aCollection [
	(aBlock value: self) ifTrue: [aCollection add: self].
	self contents do: [:sub | sub deeplySelect: aBlock in: aCollection].
	^ aCollection
]

{ #category : #menu }
SettingNode >> deleteStoredValue [
	self persistence deleteSettingNode: self
]

{ #category : #accessing }
SettingNode >> description [
	^ self settingDeclaration description
]

{ #category : #menu }
SettingNode >> exploreItem [
	self settingDeclaration inspect
]

{ #category : #accessing }
SettingNode >> helpText [
	^ self item description ifEmpty: [ nil ]
]

{ #category : #accessing }
SettingNode >> icon [
	^ self settingDeclaration ifNotNil: [:d | d icon ] 
]

{ #category : #accessing }
SettingNode >> inputMorphFor: aContainer [
	^ self settingDeclaration
		ifNotNil: [:v | 
			(v inputMorphFor: aContainer) ifNil: [ '' ]]
]

{ #category : #menu }
SettingNode >> inspectItem [
	self settingDeclaration inspect
]

{ #category : #testing }
SettingNode >> isAnotherValueStored [
	"Return true if the another setting value is stored."
	| storedValue |
	storedValue := self storedValue.
	^ storedValue notNil and: [ storedValue ~= self settingValue ]
]

{ #category : #testing }
SettingNode >> isCurrentValueStored [
	"Return true if the current setting value is stored."
	^ self storedValue = self settingValue
]

{ #category : #testing }
SettingNode >> isEmpty [
	^ self contents isEmpty
]

{ #category : #testing }
SettingNode >> isNotCurrentValueStored [
	"Return false if the current setting value is stored."
	^ self isCurrentValueStored not

]

{ #category : #testing }
SettingNode >> isStored [
	"Return true if this setting node has stored value. 
	The stored value can differ from the current value."
	^ self storedValue notNil
]

{ #category : #menu }
SettingNode >> keyStroke: anEvent from: aTreeMorph [
	| c |
	c := anEvent keyCharacter.
	c = $b
		ifTrue: [self menuBrowse. ^ true].
	c = $i
		ifTrue: [self inspectItem. ^ true].
	c = $I
		ifTrue: [self exploreItem. ^ true].
	^ false

]

{ #category : #accessing }
SettingNode >> labelMorphFor: aContainer [
	^ self settingDeclaration 
		ifNotNil: [:v | v labelMorphFor: aContainer]
]

{ #category : #menu }
SettingNode >> loadFromDisk [
	self persistence updateSettingNode: self
]

{ #category : #menu }
SettingNode >> menu: menu shifted: b [ 
	menu
		add: 'Browse (b)'
		target: self
		selector: #browseDeclaration.		
	self item isExportable ifFalse: [ ^self ].
	(menu
		add: 'Save to disk'
		target: self
		selector: #saveToDisk)
			enablementSelector: #isNotCurrentValueStored.
	(menu
		add: 'Load from disk'
		target: self
		selector: #loadFromDisk)
			enablementSelector: #isAnotherValueStored.
	(menu
		add: 'Delete stored value'
		target: self
		selector: #deleteStoredValue)
			enablementSelector: #isStored.
]

{ #category : #menu }
SettingNode >> menuBrowse [
	self browseDeclaration
]

{ #category : #accessing }
SettingNode >> methodClass [
	^ self pragma ifNotNil: [self pragma methodClass instanceSide]
]

{ #category : #accessing }
SettingNode >> order [
	^ item order
]

{ #category : #accessing }
SettingNode >> order: anInteger [
	item order: anInteger
]

{ #category : #accessing }
SettingNode >> parent: aSymbol [
	self parentName: aSymbol
]

{ #category : #accessing }
SettingNode >> parentName [
	^ parentName
	
]

{ #category : #accessing }
SettingNode >> parentName: aSymbol [
	parentName := aSymbol
	
]

{ #category : #accessing }
SettingNode >> parentNode [
	^ self model nodeNamed: self parentName
]

{ #category : #accessing }
SettingNode >> persistence [
	^ self model persistence
]

{ #category : #accessing }
SettingNode >> pragma [
	^ pragma
]

{ #category : #accessing }
SettingNode >> pragma: aPragma [
	pragma := aPragma
]

{ #category : #accessing }
SettingNode >> realValue [
	^ self settingDeclaration realValue
]

{ #category : #accessing }
SettingNode >> realValue: anObject [
	self settingDeclaration realValue: anObject
]

{ #category : #accessing }
SettingNode >> receiverPackage [
	^ self settingReceiver
		ifNotNil: [self settingReceiver class package] 
]

{ #category : #menu }
SettingNode >> saveToDisk [
	self persistence storeSettingNode: self
]

{ #category : #accessing }
SettingNode >> selector [
	^ self pragma ifNil: [ '' ] ifNotNil: [ :p | p methodSelector ]
]

{ #category : #accessing }
SettingNode >> settingDeclaration [
	^ self item
]

{ #category : #printing }
SettingNode >> settingNodeIdentifier [
	^ String streamContents: [ :aStream |
			self settingNodeIdentifierOn: aStream ]
]

{ #category : #printing }
SettingNode >> settingNodeIdentifierOn: aWriteStream [ 
	parentNode 
		ifNotNil: [ parentName settingNodeIdentifierOn: aWriteStream ]
		ifNil: [ parentName ifNotNil: [ aWriteStream print: parentName ] ].
	aWriteStream print: self settingDeclaration name
]

{ #category : #accessing }
SettingNode >> settingReceiver [
	^ item settingReceiver
]

{ #category : #accessing }
SettingNode >> settingValue [
	^ self settingDeclaration realValue

]

{ #category : #accessing }
SettingNode >> storedValue [
	"It returns a stored value for the setting node.
	It returns nil value when there is no stored value."	
	^ self persistence storedValueForSettingNode: self
]

{ #category : #updating }
SettingNode >> updateAccordingTo: aFilter [ 
	(aFilter keepSettingTreeNode: self) 
		ifFalse: [
			self contents do: [ :n | n updateAccordingTo: aFilter].
			self contents removeAllSuchThat: [ :n | (aFilter keepSettingTreeNode: n) not  and: [n isEmpty]]]
		
]
