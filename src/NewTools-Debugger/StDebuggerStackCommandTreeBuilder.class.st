"
I build the default command tree for the debugger stack presenter.
The command classes I instantiate are specified under the ""accessing - commands classes"" protocol.
The group names and descriptions are specified under the ""accessing"" protocol.

You can evolve the default groups names and descriptions and the default command classes, but you cannot extend me.
Using me to add command in the tree makes them default for the debugger stack presenter.
"
Class {
	#name : #StDebuggerStackCommandTreeBuilder,
	#superclass : #StDebuggerCommandTreeBuilder,
	#category : #'NewTools-Debugger-Model'
}

{ #category : #accessing }
StDebuggerStackCommandTreeBuilder class >> displayStrategy [
	^CmUIDisplayAsSubMenu
]

{ #category : #accessing }
StDebuggerStackCommandTreeBuilder class >> groupDescription [
	^ 'Actions on the debugger stack.'
]

{ #category : #accessing }
StDebuggerStackCommandTreeBuilder class >> groupName [ 	
	^'Stack'
]

{ #category : #accessing }
StDebuggerStackCommandTreeBuilder >> browseGroupDescription [
	^ 'Navigate the system from the stack'
]

{ #category : #accessing }
StDebuggerStackCommandTreeBuilder >> browseGroupName [
	^ 'Browse'
]

{ #category : #building }
StDebuggerStackCommandTreeBuilder >> buildDebuggerCommandGroup [
	|stackGroup|
	stackGroup := self group.	
	stackGroup decoratedGroup register: self stackContextMenuBrowseCommands.
	stackGroup decoratedGroup register: self stackContextMenuExplorationCommands.	
	stackGroup decoratedGroup register: self stackContextMenuStackEditionsCommands.
	stackGroup decoratedGroup register: self stackContextMenuActionsCommands.

	
]

{ #category : #building }
StDebuggerStackCommandTreeBuilder >> buildSpecCommand: aCommandClass forContext: anObject [
	| cmd |
	cmd := super buildSpecCommand: aCommandClass forContext: anObject.
	cmd name: aCommandClass shortName.
	^ cmd
]

{ #category : #'accessing - commands classes' }
StDebuggerStackCommandTreeBuilder >> stackActionsCommandsClasses [
	^ {
	StReturnValueCommand
	}
]

{ #category : #commands }
StDebuggerStackCommandTreeBuilder >> stackContextMenuActionsCommands [
	| group |
	group := (CmCommandGroup named: self stackContextMenuActionsGroupName)
		asSpecGroup.
	group beDisplayedAsGroup.
	group description: self stackContextMenuActionsDescription.
	group iconName: #smallDoIt.
	self stackActionsCommandsClasses
		do: [ :cmdClass | 
			group
				register: (self buildSpecCommand: cmdClass forContext: stDebuggerInstance) ].
	^ group
]

{ #category : #accessing }
StDebuggerStackCommandTreeBuilder >> stackContextMenuActionsDescription [
	^ 'Actions on the selected context'
]

{ #category : #accessing }
StDebuggerStackCommandTreeBuilder >> stackContextMenuActionsGroupName [
	^ 'Context Actions'
]

{ #category : #commands }
StDebuggerStackCommandTreeBuilder >> stackContextMenuBrowseCommands [
	| group |
	group := (CmCommandGroup named: self browseGroupName) asSpecGroup.
	group description: self browseGroupDescription.
	group iconName: #smallSystemBrowser.
	group register: self stackContextNavigationCommands.
	group register: self stackMethodNavigationCommands.
	group register: self stackMethodSelectorNavigationCommands.
	group register: self stackReceiverClassNavigationCommands.
	^ group
]

{ #category : #commands }
StDebuggerStackCommandTreeBuilder >> stackContextMenuExplorationCommands [
	| group |
	group := (CmCommandGroup
		named: self stackContextMenuExplorationGroupName) asSpecGroup.
	group description: self stackContextMenuExplorationDescription.
	group iconName: SpSystemDiscoveryCommand defaultIconName.
	self stackReceiverClassExplorationCommandsClasses
		do: [ :cmdClass | 
			group
				register:
					(self
						buildSpecCommand: cmdClass
						forContext: stDebuggerInstance stackSelectionReceiverClassContext) ].
	self stackReceiverExplorationCommandsClasses
		do: [ :cmdClass | 
			group
				register:
					(self
						buildSpecCommand: cmdClass
						forContext: stDebuggerInstance stackSelectionReceiverContext) ].
	^ group
]

{ #category : #accessing }
StDebuggerStackCommandTreeBuilder >> stackContextMenuExplorationDescription [
	^ 'Explore the receiver object''s graph'
]

{ #category : #accessing }
StDebuggerStackCommandTreeBuilder >> stackContextMenuExplorationGroupName [
	^ 'Explore'
]

{ #category : #commands }
StDebuggerStackCommandTreeBuilder >> stackContextMenuStackEditionsCommands [
	| group |
	group := (CmCommandGroup named: self stackGroupName) asSpecGroup.
	group description: self stackGroupDescription.
	group iconName: #edit.
	self stackEditionCommandsClasses
		do: [ :cmdClass | 
			group
				register: (self buildSpecCommand: cmdClass forContext: stDebuggerInstance) ].
	^ group
]

{ #category : #commands }
StDebuggerStackCommandTreeBuilder >> stackContextNavigationCommands [
	"Commands to navigate the context"

	| group |
	group := (CmCommandGroup named: self stackContextNavigationGroupName)
		asSpecGroup.
	group beDisplayedAsGroup.
	group
		register:
			((SpBrowseClassCommand
				forSpecContext: stDebuggerInstance stackSelectionMethodContext)
				name: 'method';
				shortcutKey: nil;
				yourself).
	group
		register:
			((SpBrowseClassCommand
				forSpecContext: stDebuggerInstance stackSelectionReceiverClassContext)
				name: 'receiver';
				shortcutKey: nil;
				yourself).
	^ group
]

{ #category : #accessing }
StDebuggerStackCommandTreeBuilder >> stackContextNavigationGroupName [
	^ 'Receiver class code navigation commands'
]

{ #category : #'accessing - commands classes' }
StDebuggerStackCommandTreeBuilder >> stackEditionCommandsClasses [
	^ {
	StCopyStackToClipboardCommand.
	StShowFullStackCommand.
	StPeelToFirstCommand.
	StFileOutMethodCommand
	}
]

{ #category : #'accessing - commands classes' }
StDebuggerStackCommandTreeBuilder >> stackExplorationCommandsClasses [
	^ OrderedCollection new
		addAll: self stackReceiverClassExplorationCommandsClasses;
		addAll: self stackReceiverExplorationCommandsClasses;
		yourself
]

{ #category : #accessing }
StDebuggerStackCommandTreeBuilder >> stackGroupDescription [
	^ 'Stack edition'
]

{ #category : #accessing }
StDebuggerStackCommandTreeBuilder >> stackGroupName [
	^ 'Stack'
]

{ #category : #'accessing - commands classes' }
StDebuggerStackCommandTreeBuilder >> stackMethodCommandsClasses [
	^ {
	SpBrowseMethodInheritanceCommand.
	SpBrowseMethodVersionsCommand
	}
]

{ #category : #commands }
StDebuggerStackCommandTreeBuilder >> stackMethodNavigationCommands [
	"Commands to navigate the selected context's method "

	| group |
	group := (CmCommandGroup named: self stackMethodNavigationGroupName)
		asSpecGroup.
	group beDisplayedAsGroup.
	self stackMethodCommandsClasses
		do: [ :navCmdClass | 
			group
				register:
					(self
						buildSpecCommand: navCmdClass
						forContext: stDebuggerInstance stackSelectionMethodContext) ].
	^ group
]

{ #category : #accessing }
StDebuggerStackCommandTreeBuilder >> stackMethodNavigationGroupName [
	^ 'Selected method navigation commands'
]

{ #category : #'accessing - commands classes' }
StDebuggerStackCommandTreeBuilder >> stackMethodSelectorCommandsClasses [
	^ {
	SpBrowseImplementorsCommand.
	SpBrowseSendersCommand.
	SpBrowseMethodReferencesCommand
	}
]

{ #category : #commands }
StDebuggerStackCommandTreeBuilder >> stackMethodSelectorNavigationCommands [
	"Commands to navigate the selected context's method selector as context "

	| group |
	group := (CmCommandGroup
		named: self stackMethodSelectorNavigationGroupName) asSpecGroup.
	group beDisplayedAsGroup.
	self stackMethodSelectorCommandsClasses
		do: [ :navCmdClass | 
			group
				register:
					(self
						buildSpecCommand: navCmdClass
						forContext: stDebuggerInstance stackSelectionMethodSelectorContext) ].
	^ group
]

{ #category : #accessing }
StDebuggerStackCommandTreeBuilder >> stackMethodSelectorNavigationGroupName [
	^ 'Selected method''s selector navigation commands'
]

{ #category : #'accessing - commands classes' }
StDebuggerStackCommandTreeBuilder >> stackReceiverClassExplorationCommandsClasses [
	^ {
	SpBrowseInstancesCommand.
	SpBrowseSubInstancesCommand.
	}
]

{ #category : #commands }
StDebuggerStackCommandTreeBuilder >> stackReceiverClassNavigationCommands [
	"Commands to navigate the selected context's receiver class as context "

	| group |
	group := (CmCommandGroup
		named: self stackReceiverClassNavigationGroupName) asSpecGroup.
	group beDisplayedAsGroup.
	self stackReceiverCommandsClasses
		do: [ :navCmdClass | 
			group
				register:
					(self
						buildSpecCommand: navCmdClass
						forContext: stDebuggerInstance stackSelectionReceiverClassContext) ].
	^ group
]

{ #category : #accessing }
StDebuggerStackCommandTreeBuilder >> stackReceiverClassNavigationGroupName [
	^ 'Receiver class navigation commands'
]

{ #category : #'accessing - commands classes' }
StDebuggerStackCommandTreeBuilder >> stackReceiverCommandsClasses [
	^ {
	SpBrowseClassReferencesCommand.
	SpBrowseClassHierarchyCommand.
	SpBrowseClassVariablesCommand.
	SpBrowseClassVarRefsCommand.
	SpBrowseInstVarRefsCommand
	}
]

{ #category : #'accessing - commands classes' }
StDebuggerStackCommandTreeBuilder >> stackReceiverExplorationCommandsClasses [
	^ {SpBrowsePointersToCommand}
]
