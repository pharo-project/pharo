"
I am a command to open a critic browser on given set of packages
"
Class {
	#name : #ClyOpenCriticBrowserCommand,
	#superclass : #SycPackageCommand,
	#category : #'Calypso-SystemPlugins-Critic-Browser'
}

{ #category : #activation }
ClyOpenCriticBrowserCommand class >> packageContextMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation 
		byItemOf: ClyQueryMenuGroup for: RPackage asCalypsoItemContext 
]

{ #category : #activation }
ClyOpenCriticBrowserCommand >> defaultMenuItemName [
	^'Browse critiques'
]

{ #category : #execution }
ClyOpenCriticBrowserCommand >> execute [

	CriticBrowser
		openOnRule: (ReRuleManager uniqueInstance allRules) 
		onEnvironment: (RBPackageEnvironment packages: packages)
]
