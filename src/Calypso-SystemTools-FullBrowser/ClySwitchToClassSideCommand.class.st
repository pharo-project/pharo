"
I am a command to switch full browser to the class side mode
"
Class {
	#name : 'ClySwitchToClassSideCommand',
	#superclass : 'ClySwitchClassMetaLevelCommand',
	#category : 'Calypso-SystemTools-FullBrowser-Commands-MetaLevel',
	#package : 'Calypso-SystemTools-FullBrowser',
	#tag : 'Commands-MetaLevel'
}

{ #category : 'activation' }
ClySwitchToClassSideCommand class >> toolbarOrder [
	^2
]

{ #category : 'execution' }
ClySwitchToClassSideCommand >> metaLevelScope [
	^ClyClassSideScope
]
