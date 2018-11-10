"
I represent class side methods with pragma <sampleInstance>.
After method execution I open inspector on the result
"
Class {
	#name : #ClySampleInstanceScript,
	#superclass : #ClyClassScript,
	#category : #'Calypso-SystemPlugins-ClassScripts-Queries'
}

{ #category : #'script detection' }
ClySampleInstanceScript class >> isImplementedByMethod: aMethod [
	^aMethod hasPragmaNamed: #sampleInstance
]

{ #category : #accessing }
ClySampleInstanceScript >> description [
	^'Get the class sample (predefined instance) in a inspector to tweak it and have fun with it'
]

{ #category : #execution }
ClySampleInstanceScript >> executeBy: aClass [

	| sample |
	sample := super executeBy: aClass.
	sample inspect
]

{ #category : #accessing }
ClySampleInstanceScript >> iconName [
	^#smallDoItIcon
]
