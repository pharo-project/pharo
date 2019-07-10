"
I represent the action of the performed on class side methods marked with pragma <example> or with selector beginning with example word.

example1

	BorderedMorph new openInWorld 
	
or 

foo
	<example>
	
	BorderedMorph new openInWorld
	
will open both a new morph in the world. 

Note that if you add in addition the pragma <sampleInstance> you will get an inspector on the result. 
"
Class {
	#name : #ClyExampleScript,
	#superclass : #ClyClassScript,
	#category : #'Calypso-SystemPlugins-ClassScripts-Queries'
}

{ #category : #'script detection' }
ClyExampleScript class >> isImplementedByMethod: aMethod [
	(aMethod hasPragmaNamed: #sampleInstance) ifTrue: [ ^ false ].
	^ (aMethod selector beginsWith: 'example') 
		 or: [ aMethod hasPragmaNamed: #example ]
]

{ #category : #accessing }
ClyExampleScript >> description [
	^ 'Run the example'
]

{ #category : #accessing }
ClyExampleScript >> iconName [
	^#smallDoItIcon
]
