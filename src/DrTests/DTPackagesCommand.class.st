"
I am an abstract command concerning DrTests' package list.
"
Class {
	#name : 'DTPackagesCommand',
	#superclass : 'DTCommand',
	#category : 'DrTests-Commands',
	#package : 'DrTests',
	#tag : 'Commands'
}

{ #category : 'testing' }
DTPackagesCommand class >> isAbstract [

	^ self name = #DTPackagesCommand
]
