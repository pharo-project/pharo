"
I a command to run particular script by given class.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	script:		<ClyClassScript>
	targetClass:		<Class>

"
Class {
	#name : #ClyRunClassScriptCommand,
	#superclass : #CmdCommand,
	#instVars : [
		'targetClass',
		'script'
	],
	#category : #'Calypso-SystemPlugins-ClassScripts-Browser'
}

{ #category : #testing }
ClyRunClassScriptCommand class >> canBeExecutedInContext: aBrowserContext [
	(super canBeExecutedInContext: aBrowserContext) ifFalse: [ ^false ].
	
	^aBrowserContext isClassSelected 
		and: [ aBrowserContext isClassScriptSelected ]
	
]

{ #category : #activation }
ClyRunClassScriptCommand class >> methodTableIconActivation [
	<classAnnotation>
	
	^ClyTableIconCommandActivation for: ClyMethod asCalypsoItemContext 
]

{ #category : #accessing }
ClyRunClassScriptCommand >> defaultMenuIcon [
	^Smalltalk ui iconNamed: script iconName
]

{ #category : #accessing }
ClyRunClassScriptCommand >> defaultMenuIconName [
	^#scriptManagerIcon
]

{ #category : #accessing }
ClyRunClassScriptCommand >> defaultMenuItemName [
	^'Run script'
]

{ #category : #execution }
ClyRunClassScriptCommand >> execute [
	script executeOn: targetClass
]

{ #category : #execution }
ClyRunClassScriptCommand >> prepareFullExecutionInContext: aBrowserContext [
	super prepareFullExecutionInContext: aBrowserContext.
	
	script := aBrowserContext selectedClassScript.
	targetClass := aBrowserContext selectedClasses
		detect: [ :each | script isImplementedByClass: each ]
		ifNone: [ script implementorClass ]
]

{ #category : #execution }
ClyRunClassScriptCommand >> readParametersFromContext: aBrowserContext [
	super readParametersFromContext: aBrowserContext.
	
	script := aBrowserContext selectedClassScript 
]
