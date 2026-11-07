"
I a command to run particular script by given class.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	script:		<ClyClassScript>
	targetClass:		<Class>

"
Class {
	#name : 'ClyRunClassScriptCommand',
	#superclass : 'SycClassCommand',
	#instVars : [
		'targetClass',
		'script'
	],
	#category : 'Calypso-SystemPlugins-ClassScripts-Browser',
	#package : 'Calypso-SystemPlugins-ClassScripts-Browser'
}

{ #category : 'testing' }
ClyRunClassScriptCommand class >> canBeExecutedInContext: aBrowserContext [
	(super canBeExecutedInContext: aBrowserContext) ifFalse: [ ^false ].

	^aBrowserContext isClassSelected
		and: [ aBrowserContext isClassScriptSelected ]
]

{ #category : 'activation' }
ClyRunClassScriptCommand class >> methodTableIconActivation [
	<classAnnotation>

	^ClyTableIconCommandActivation for: ClyMethod asCalypsoItemContext
]

{ #category : 'execution' }
ClyRunClassScriptCommand >> executeRefactoring [
	self script executeOn: targetClass
]

{ #category : 'accessing' }
ClyRunClassScriptCommand >> iconName [
	^ self script iconName
]

{ #category : 'accessing' }
ClyRunClassScriptCommand >> name [
	^'Run script'
]

{ #category : 'execution' }
ClyRunClassScriptCommand >> prepareFullExecution [

	super prepareFullExecution.
	targetClass := self context selectedClasses
		               detect: [ :each | self script isImplementedByClass: each ]
		               ifNone: [ self script implementorClass ]
]

{ #category : 'accessing' }
ClyRunClassScriptCommand >> script [
	^ script ifNil: [ self context ifNotNil: [ :ctx | ctx selectedClassScript ] ]
]
