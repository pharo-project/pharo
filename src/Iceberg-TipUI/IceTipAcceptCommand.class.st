"
I'm a generic accept action command. 
My user needs to implement the method #accept.
"
Class {
	#name : #IceTipAcceptCommand,
	#superclass : #IceTipCommand,
	#instVars : [
		'browser'
	],
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #activation }
IceTipAcceptCommand class >> checkoutShortcutActivation [
	<classAnnotation>
	
	^ CmdShortcutActivation by: $s meta for: IceTipBranchContext
]

{ #category : #activation }
IceTipAcceptCommand class >> commitShortcutActivation [
	<classAnnotation>
	
	^ CmdShortcutActivation by: $s meta for: IceTipCommitContext
]

{ #category : #accessing }
IceTipAcceptCommand class >> defaultHelp [
	^ 'Accept the current action'
]

{ #category : #activation }
IceTipAcceptCommand class >> pullShortcutActivation [
	<classAnnotation>
	
	^ CmdShortcutActivation by: $s meta for: IceTipPullContext
]

{ #category : #activation }
IceTipAcceptCommand class >> pushShortcutActivation [
	<classAnnotation>
	
	^ CmdShortcutActivation by: $s meta for: IceTipPushContext
]

{ #category : #execution }
IceTipAcceptCommand >> execute [
	browser accept
]

{ #category : #execution }
IceTipAcceptCommand >> readParametersFromContext: aToolContext [
	super readParametersFromContext: aToolContext.
	browser := aToolContext tool
]
