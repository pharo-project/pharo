"
I am a command that can be executed in the critic context (ClyCriticContext) and that will allows one to browser the class of a rule in the system.

This might be useful in case a user want to understand a rule from the code.
	
 
Internal Representation and Key Implementation Points.
--------------------

    Instance Variables
	criticClass:		<aClass> 		The critic class to browse.
"
Class {
	#name : #ClyBrowseRuleCommand,
	#superclass : #ClyBrowserCommand,
	#instVars : [
		'criticClass'
	],
	#category : #'Calypso-SystemPlugins-Critic-Browser'
}

{ #category : #activation }
ClyBrowseRuleCommand class >> browserContextMenuActivation [
	<classAnnotation>
	^ CmdContextMenuActivation byItemOf: CmdExtraMenuGroup for: ClyCriticContext
]

{ #category : #accessing }
ClyBrowseRuleCommand >> defaultMenuIconName [
	^ #smallSystemBrowser
]

{ #category : #accessing }
ClyBrowseRuleCommand >> defaultMenuItemName [
	^ 'Browse rule'
]

{ #category : #execution }
ClyBrowseRuleCommand >> execute [
	criticClass browse
]

{ #category : #execution }
ClyBrowseRuleCommand >> prepareFullExecutionInContext: aContext [
	super prepareFullExecutionInContext: aContext.

	criticClass := aContext selectedCritique rule class
]
