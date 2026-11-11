"
I am the group of the Extra commands in a Calypso context menu. I am a plain group of the tree, without any own item.
"
Class {
	#name : 'ClyExtraMenuGroup',
	#superclass : 'ClyMenuGroup',
	#category : 'Calypso-Commands',
	#package : 'Calypso-Commands'
}

{ #category : 'accessing' }
ClyExtraMenuGroup >> description [
	^'Rarely used commands'
]

{ #category : 'presenting' }
ClyExtraMenuGroup >> hasLineBefore [
	^ true
]

{ #category : 'accessing' }
ClyExtraMenuGroup >> name [
	^'Extra'
]

{ #category : 'accessing' }
ClyExtraMenuGroup >> order [
	"Idea to put extra group to the end of menu"
	^SmallInteger maxVal
]
