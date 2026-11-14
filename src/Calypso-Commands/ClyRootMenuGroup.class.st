"
I am the root group of a Calypso context menu tree. The commands declared without a menu group are in me.
"
Class {
	#name : 'ClyRootMenuGroup',
	#superclass : 'ClyMenuGroup',
	#instVars : [
		'name'
	],
	#category : 'Calypso-Commands',
	#package : 'Calypso-Commands'
}

{ #category : 'parents' }
ClyRootMenuGroup class >> parentGroup [
	self error: 'should not be called'
]

{ #category : 'testing' }
ClyRootMenuGroup >> isInlined [
	"Root group is never presented as separate item in the list"
	^true
]

{ #category : 'testing' }
ClyRootMenuGroup >> isRoot [
	^true
]

{ #category : 'accessing' }
ClyRootMenuGroup >> name [
	^ name ifNil: [ super name ]
]

{ #category : 'accessing' }
ClyRootMenuGroup >> name: anObject [
	name := anObject
]
