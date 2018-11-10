"
This command represent the commands with submenus to select remotes
"
Class {
	#name : #IceGitHubBranchSelectCommand,
	#superclass : #IceGitHubCommand,
	#instVars : [
		'remote'
	],
	#category : #'Iceberg-Plugin-GitHub-Commands'
}

{ #category : #accessing }
IceGitHubBranchSelectCommand >> defaultMenuItemName [
	^ remote ifNotNil: [ remote name , ' (' , remote url , ')' ] ifNil: [ self itemNameWithoutBranches ]
]

{ #category : #'context menu' }
IceGitHubBranchSelectCommand >> fillContextMenu: aMenu using: aCommandActivator [

	| concreteScopeCommand switchActivator subMenu |

	subMenu := MenuMorph new.
	self repository remotes do: [ :each |  
		concreteScopeCommand := self copy remote: each; yourself.
		switchActivator := aCommandActivator newForCommand: concreteScopeCommand.
		subMenu add: switchActivator menuItemName target: switchActivator selector: #executeCommand		
	].
	
	aMenu add: self defaultMenuItemName subMenu: subMenu.
]

{ #category : #accessing }
IceGitHubBranchSelectCommand >> itemNameWithoutBranches [
	self subclassResponsibility 
]

{ #category : #accessing }
IceGitHubBranchSelectCommand >> remote [
	^ remote ifNil: [ remote := self askRemote ]
]

{ #category : #accessing }
IceGitHubBranchSelectCommand >> remote: aRemote [
	^ remote := aRemote
]
