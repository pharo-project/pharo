"
I represent class side method #initialize
"
Class {
	#name : #ClyClassInitializationScript,
	#superclass : #ClyClassScript,
	#category : #'Calypso-SystemPlugins-ClassScripts-Queries'
}

{ #category : #'script detection' }
ClyClassInitializationScript class >> isImplementedByMethod: aMethod [

	^ aMethod selector == #initialize
]

{ #category : #accessing }
ClyClassInitializationScript >> description [
	^ '(Re)initialize class'
]
