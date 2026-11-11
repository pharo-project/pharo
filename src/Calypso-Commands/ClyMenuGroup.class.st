"
I am a group of a Calypso context menu tree. I hold the items and the nested groups, sorted by their order and name. My subclasses are the groups which place the commands of a context menu.
"
Class {
	#name : 'ClyMenuGroup',
	#superclass : 'Object',
	#category : 'Calypso-Commands',
	#package : 'Calypso-Commands'
}

{ #category : 'parents' }
ClyMenuGroup class >> parentGroup [
	^ClyRootMenuGroup
]

{ #category : 'accessing' }
ClyMenuGroup >> description [
	"By default the result is nil which is not really nice
	but it is already expected by many tools. So it is easy to allow nil"
	^nil
]

{ #category : 'presenting' }
ClyMenuGroup >> hasLineAfter [
	^ false
]

{ #category : 'presenting' }
ClyMenuGroup >> hasLineBefore [
	"Lines drawn around this group in a context menu (see ClyCommandGroupMenuRenderer)."
	^ false
]

{ #category : 'accessing' }
ClyMenuGroup >> icon [
	"By default the result is nil which is not really nice
	but it is already expected by many tools.	So it is easy to allow nil.
	For simple cases users should implement method #iconName"
	^self iconName
		ifNotNil: [:iconName | Smalltalk ui iconNamed: iconName ]
]

{ #category : 'accessing' }
ClyMenuGroup >> iconName [
	"Not really nice to return nil. But it is already expected by many tools.
	So it is easy to allow nil.
	Users should use method #icon which returns Form instance"

	^nil
]

{ #category : 'testing' }
ClyMenuGroup >> isInlined [
	^false
]

{ #category : 'testing' }
ClyMenuGroup >> isRoot [
	^false
]

{ #category : 'accessing' }
ClyMenuGroup >> name [
	^self class name
]

{ #category : 'accessing' }
ClyMenuGroup >> order [
	^500
]
