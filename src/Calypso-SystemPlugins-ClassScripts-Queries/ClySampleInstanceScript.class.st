"
I represent the action of the performed on class side methods marked with pragma <sampleInstance>.
After method execution I open an inspector on the result.
Note that if your method is named 

[[[  
example1
	<sampleInstance>
	^ 42
]]]

It will still open an inspector even if the browser consider them as example (see comments of the ClyExampleScript class)
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
	^ 'Get the class sample (predefined instance) in a inspector to tweak it and have fun with it'
]

{ #category : #execution }
ClySampleInstanceScript >> executeOn: aClass [

 	(super executeOn: aClass) inspect
]

{ #category : #accessing }
ClySampleInstanceScript >> iconName [
	^ #smallDoItIcon
]
