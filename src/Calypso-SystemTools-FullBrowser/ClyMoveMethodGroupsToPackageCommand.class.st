"
I am a command to move all methods of given method groups to another package.

Internal Representation and Key Implementation Points.

    Instance Variables
	package:		<RPackage>
"
Class {
	#name : #ClyMoveMethodGroupsToPackageCommand,
	#superclass : #ClyMethodGroupCommand,
	#instVars : [
		'package'
	],
	#category : #'Calypso-SystemTools-FullBrowser-Commands-MethodGroups'
}

{ #category : #activation }
ClyMoveMethodGroupsToPackageCommand class >> fullBrowserContextMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemFor: ClyMethodGroup asCalypsoItemContext 
]

{ #category : #activation }
ClyMoveMethodGroupsToPackageCommand class >> fullBrowserDragAndDropActivation [
	<classAnnotation>
	
	^CmdDragAndDropActivation 
		for: ClyMethodGroup asCalypsoItemContext 
		toDropIn: RPackage asCalypsoItemContext
]

{ #category : #accessing }
ClyMoveMethodGroupsToPackageCommand >> defaultMenuItemName [

	^'Convert to extension'
]

{ #category : #execution }
ClyMoveMethodGroupsToPackageCommand >> execute [
	
	| repackagingCommand |
	methodGroups do: [ :each | 
		repackagingCommand := SycMoveMethodsToPackageCommand for: each methods to: package.
		repackagingCommand execute.
		each removeWithMethods]
]

{ #category : #accessing }
ClyMoveMethodGroupsToPackageCommand >> package [
	^ package
]

{ #category : #accessing }
ClyMoveMethodGroupsToPackageCommand >> package: anObject [
	package := anObject
]

{ #category : #execution }
ClyMoveMethodGroupsToPackageCommand >> prepareExecutionInDropContext: aToolContext [
	super prepareExecutionInDropContext: aToolContext.
	package := aToolContext lastSelectedPackage
]

{ #category : #execution }
ClyMoveMethodGroupsToPackageCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	
	package := aToolContext requestSinglePackage: 'Choose package'
]
