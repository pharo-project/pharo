"
I represent class side methods with pragma <example> or with selector beginning with example word
"
Class {
	#name : #ClyExampleScript,
	#superclass : #ClyClassScript,
	#category : #'Calypso-SystemPlugins-ClassScripts-Queries'
}

{ #category : #'script detection' }
ClyExampleScript class >> isImplementedByMethod: aMethod [
	^(aMethod selector beginsWith: 'example') 
		or: [aMethod hasPragmaNamed: #example]
]

{ #category : #accessing }
ClyExampleScript >> description [
	^'Run the example'
]

{ #category : #accessing }
ClyExampleScript >> iconName [
	^#smallDoItIcon
]
