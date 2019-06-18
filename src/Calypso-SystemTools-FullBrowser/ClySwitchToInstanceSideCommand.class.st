"
I am a command to switch full browser to the instance side mode
"
Class {
	#name : #ClySwitchToInstanceSideCommand,
	#superclass : #ClySwitchClassMetaLevelCommand,
	#category : #'Calypso-SystemTools-FullBrowser-Commands-MetaLevel'
}

{ #category : #activation }
ClySwitchToInstanceSideCommand class >> toolbarOrder [
	^1
]

{ #category : #execution }
ClySwitchToInstanceSideCommand >> metaLevelScope [
	^ClyInstanceSideScope 
]
