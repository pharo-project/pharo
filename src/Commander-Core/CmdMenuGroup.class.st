"
I am a root of hierarchy of first class menu groups.
My subclasses are used to declare command position in menu. They are declared in menu command activation strategies.
My own menu position is defined statically by instance side method #order and class side method #parentGroup. By default #parentGroup is CmdRootMenuGroup which represents root menu items. If some group wants to be in deep tree structure it overrides #parentGroup method to return another group class. 

My instances contain other menu items (commands and groups). I provide accessing methods for them:
- addItem: aMenuItem
- removeItem: aMenuItem
- includes: aMenuItem
- size

Different kinds of menu activation strategies extend me to build different kind of menu (context menu, toobar, halo menu, etc.)

Internal Representation and Key Implementation Points.

    Instance Variables
	contents:		<OrderedCollection of<CmdMenuItem>>
"
Class {
	#name : #CmdMenuGroup,
	#superclass : #CmdMenuItem,
	#instVars : [
		'contents'
	],
	#category : #'Commander-Core'
}

{ #category : #parents }
CmdMenuGroup class >> parentGroup [
	^CmdRootMenuGroup 
]

{ #category : #accessing }
CmdMenuGroup >> addItem: aMenuItem [
	aMenuItem parentGroup: self.
	contents add: aMenuItem
]

{ #category : #accessing }
CmdMenuGroup >> allGroupsWhich: aBlock [ 
	| result |
	result := OrderedCollection new.
	
	contents 
		reject: [ :each | each isCommandItem ]
		thenDo: [ :each | 
			(aBlock value: each) ifTrue: [ result add: each ].
			result addAll: (each allGroupsWhich: aBlock) ].
		
	^result
]

{ #category : #accessing }
CmdMenuGroup >> allItemsWhich: aBlock [ 
	| result |
	result := OrderedCollection new.
	
	contents do: [ :each | 
		(aBlock value: each) ifTrue: [ result add: each ].
		each isCommandItem ifFalse: [ 
			result addAll: (each allItemsWhich: aBlock) ]].
		
	^result
]

{ #category : #accessing }
CmdMenuGroup >> commandItemLike: aCommandClass [

	^contents detect: [ :each | 
		each isCommandItem and: [ each includesCommand: aCommandClass ]]
]

{ #category : #accessing }
CmdMenuGroup >> commands [
	^contents 
		select: [ :each | each isCommandItem ] 
		thenCollect: [ :each | each command ]
]

{ #category : #accessing }
CmdMenuGroup >> contents [
	^ contents
]

{ #category : #accessing }
CmdMenuGroup >> contents: anObject [
	contents := anObject
]

{ #category : #accessing }
CmdMenuGroup >> description [
	"By default the result is nil which is not really nice 
	but it is already expected by many tools. So it is easy to allow nil"
	^nil
]

{ #category : #accessing }
CmdMenuGroup >> findGroupLike: menuGroupClass ifExists: foundBlock [

	contents detect: [ :each | each isKindOf: menuGroupClass] ifFound: foundBlock.
		
	contents do: [ :each | each findGroupLike: menuGroupClass ifExists: foundBlock ]
]

{ #category : #accessing }
CmdMenuGroup >> findItemSimilarTo: aMenuItem ifPresent: presentBlock ifAbsent: absentBlock [

	contents 
		detect: [:each | each isSimilarTo: aMenuItem  ]
		ifFound: presentBlock
		ifNone: absentBlock
]

{ #category : #accessing }
CmdMenuGroup >> icon [
	"By default the result is nil which is not really nice 
	but it is already expected by many tools.	So it is easy to allow nil.
	For simple cases users should implement method #iconName"
	^self iconName 
		ifNotNil: [:iconName | Smalltalk ui iconNamed: iconName ]
]

{ #category : #accessing }
CmdMenuGroup >> iconName [
	"Not really nice to return nil. But it is already expected by many tools.
	So it is easy to allow nil.
	Users should use method #icon which returns Form instance"
	
	^nil
]

{ #category : #testing }
CmdMenuGroup >> includes: aMenuItem [
	^contents anySatisfy: [ :each | each isSimilarTo: aMenuItem ]
]

{ #category : #testing }
CmdMenuGroup >> includesCommand: aClass [ 
	"It answers only about itself without looking recursively into child groups"
	^contents anySatisfy: [:each |
		each isCommandItem and: [each includesCommand: aClass] ]
]

{ #category : #initialization }
CmdMenuGroup >> initialize [
	super initialize.
	contents := SortedCollection sortBlock: [ :a :b | 
		a order = b order 
			ifTrue: [ a name <= b name ]
			ifFalse: [ a order <= b order ]]
]

{ #category : #testing }
CmdMenuGroup >> isActive [
	^contents anySatisfy: [ :each | each isActive ]
]

{ #category : #testing }
CmdMenuGroup >> isCommandItem [
	^false
]

{ #category : #testing }
CmdMenuGroup >> isEmpty [
	^contents isEmpty 
]

{ #category : #testing }
CmdMenuGroup >> isInlined [
	^false
]

{ #category : #testing }
CmdMenuGroup >> isLastActiveMenuItem: childMenuItem [

	contents reverseDo: [ :each | 
		each isActive ifTrue: [ ^each == childMenuItem ] ].
	
	^false
]

{ #category : #testing }
CmdMenuGroup >> isLastMenuItem: childMenuItem [

	^contents last == childMenuItem and: [ self isLastInMenu ]
]

{ #category : #testing }
CmdMenuGroup >> isRoot [
	^false
]

{ #category : #testing }
CmdMenuGroup >> isSimilarTo: aMenuItem [
	^self class == aMenuItem class
]

{ #category : #accessing }
CmdMenuGroup >> name [
	^self class name
]

{ #category : #accessing }
CmdMenuGroup >> order [
	^500
]

{ #category : #accessing }
CmdMenuGroup >> removeItem: aGroupOrCommand [
	contents remove: aGroupOrCommand 
]

{ #category : #accessing }
CmdMenuGroup >> size [
	^contents size
]

{ #category : #updating }
CmdMenuGroup >> synchronizeContentWith: anotherGroup [

	contents removeAllSuchThat: [ :each | (anotherGroup includes: each) not].
	anotherGroup contents do: [ :each | 
		self 
			findItemSimilarTo: each
			ifPresent: [ :existing | 
				existing synchronizeContentWith: each.
				existing isEmpty ifTrue: [ self removeItem: existing ] ]
			ifAbsent: [ self addItem: each]]
]
