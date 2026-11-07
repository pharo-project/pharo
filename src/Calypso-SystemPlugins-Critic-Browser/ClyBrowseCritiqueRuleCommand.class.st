"
I am a command that can be executed in the critic context (ClyCriticContext) and that will allows one to browser the class of a rule in the system.

This might be useful in case a user want to understand a rule from the code.
	
 
Internal Representation and Key Implementation Points.
--------------------

    Instance Variables
	criticClass:		<aClass> 		The critic class to browse.
"
Class {
	#name : 'ClyBrowseCritiqueRuleCommand',
	#superclass : 'ClyCritiqueCommand',
	#category : 'Calypso-SystemPlugins-Critic-Browser',
	#package : 'Calypso-SystemPlugins-Critic-Browser'
}

{ #category : 'activation' }
ClyBrowseCritiqueRuleCommand class >> browserContextMenuActivation [
	<classAnnotation>
	^ ClyContextMenuActivation byItemOf: ClyExtraMenuGroup for: ClyCriticContext
]

{ #category : 'activation' }
ClyBrowseCritiqueRuleCommand class >> criticTableIconActivation [
	"We do not want this command to appear in the command icons next to critiques, so we override the method."
]

{ #category : 'table decoration' }
ClyBrowseCritiqueRuleCommand >> description [
	^ 'Command to browse the class defining the rule producing the critique.'
]

{ #category : 'execution' }
ClyBrowseCritiqueRuleCommand >> executeRefactoring [
	self critique rule class browse
]

{ #category : 'accessing' }
ClyBrowseCritiqueRuleCommand >> iconName [
	^ #smallSystemBrowser
]

{ #category : 'accessing' }
ClyBrowseCritiqueRuleCommand >> name [
	^ 'Browse rule'
]
