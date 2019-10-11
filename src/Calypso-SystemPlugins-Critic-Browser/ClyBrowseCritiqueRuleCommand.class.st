"
I am a command that can be executed in the critic context (ClyCriticContext) and that will allows one to browser the class of a rule in the system.

This might be useful in case a user want to understand a rule from the code.
	
 
Internal Representation and Key Implementation Points.
--------------------

    Instance Variables
	criticClass:		<aClass> 		The critic class to browse.
"
Class {
	#name : #ClyBrowseCritiqueRuleCommand,
	#superclass : #ClyCritiqueCommand,
	#category : #'Calypso-SystemPlugins-Critic-Browser'
}

{ #category : #activation }
ClyBrowseCritiqueRuleCommand class >> browserContextMenuActivation [
	<classAnnotation>
	^ CmdContextMenuActivation byItemOf: CmdExtraMenuGroup for: ClyCriticContext
]

{ #category : #activation }
ClyBrowseCritiqueRuleCommand class >> criticTableIconActivation [
	"We do not want this command to appear in the command icons next to critiques, so we override the method."

	
]

{ #category : #accessing }
ClyBrowseCritiqueRuleCommand >> defaultMenuIconName [
	^ #smallSystemBrowser
]

{ #category : #accessing }
ClyBrowseCritiqueRuleCommand >> defaultMenuItemName [
	^ 'Browse rule'
]

{ #category : #'table decoration' }
ClyBrowseCritiqueRuleCommand >> description [
	^ 'Command to browse the class defining the rule producing the critique.'
]

{ #category : #execution }
ClyBrowseCritiqueRuleCommand >> execute [
	self critique rule class browse
]
