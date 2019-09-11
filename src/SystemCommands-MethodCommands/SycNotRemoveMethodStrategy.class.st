"
I am simple cancel method remove operation by doing nothing
"
Class {
	#name : #SycNotRemoveMethodStrategy,
	#superclass : #SycRemoveMethodStrategy,
	#category : #'SystemCommands-MethodCommands'
}

{ #category : #execution }
SycNotRemoveMethodStrategy >> removeMethods: methods [

]

{ #category : #execution }
SycNotRemoveMethodStrategy >> userRequestString [
	^'Forget it -- do nothing -- sorry I asked'
]
