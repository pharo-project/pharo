"
I am an abstract command concerning DrTests' results tree but I can only be executed if a leaf is selected by user.
"
Class {
	#name : 'DTLeafResultCommand',
	#superclass : 'DTResultCommand',
	#category : 'DrTests-Commands',
	#package : 'DrTests',
	#tag : 'Commands'
}

{ #category : 'testing' }
DTLeafResultCommand class >> isAbstract [

	^ self name = #DTLeafResultCommand
]

{ #category : 'hooks' }
DTLeafResultCommand >> canBeRun [
	^ self resultSelected isLeaf
]
