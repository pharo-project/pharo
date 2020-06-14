"
An AbstractTemporariesListCommand is  command that creates | ``@temporaries |

"
Class {
	#name : #AbstractTemporariesListCommand,
	#superclass : #Command,
	#category : #'RewriteTool-Command'
}

{ #category : #testing }
AbstractTemporariesListCommand class >> isAccepted: aNode [
	^ aNode isInTemporariesList
]

{ #category : #testing }
AbstractTemporariesListCommand class >> isSequenceNeeded [
	^ true
]

{ #category : #testing }
AbstractTemporariesListCommand class >> isTemps [
	^ true
]

{ #category : #accessing }
AbstractTemporariesListCommand >> abstractName [
	| tempNodes |
	tempNodes := self panel parse: self commandReceiver formattedCode.
	^ self commandReceiver formattedCode
		copyReplaceAll: (tempNodes formattedCode copyFrom: tempNodes leftBar to: tempNodes rightBar)
		with: '| `@temporaries |'
		asTokens: false
]

{ #category : #execute }
AbstractTemporariesListCommand >> execute [
	| newNode temps |
	temps := self panel chooseTempsForNode: self commandReceiver.
	temps
		ifNil: [ 
			temps := OrderedCollection new.
			temps add: (RBVariableNode named: '``@temporaries', self panel tempListIndex asString) ].
	newNode := self commandReceiver temporaries: temps asOrderedCollection.
	^ newNode
]
