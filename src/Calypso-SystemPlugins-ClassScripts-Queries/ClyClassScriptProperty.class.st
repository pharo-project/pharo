"
I am used to extend method items with information about script which they represent.

To create my instances use following method: 

	ClyClassScriptProperty script: aScript

Internal Representation and Key Implementation Points.

    Instance Variables
	script:		<ClyClassScript>
"
Class {
	#name : #ClyClassScriptProperty,
	#superclass : #ClyProperty,
	#instVars : [
		'script'
	],
	#category : #'Calypso-SystemPlugins-ClassScripts-Queries'
}

{ #category : #'instance creation' }
ClyClassScriptProperty class >> script: aClassScript [
	^self new 
		script: aClassScript 
]

{ #category : #accessing }
ClyClassScriptProperty >> script [
	^ script
]

{ #category : #accessing }
ClyClassScriptProperty >> script: anObject [
	script := anObject
]
