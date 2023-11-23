"
I am a command to open a critic browser on given set of packages
"
Class {
	#name : 'ClyOpenCriticBrowserCommand',
	#superclass : 'SycPackageCommand',
	#category : 'Calypso-SystemPlugins-Critic-Browser',
	#package : 'Calypso-SystemPlugins-Critic-Browser'
}

{ #category : 'activation' }
ClyOpenCriticBrowserCommand class >> packageContextMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation
		byItemOf: ClyQueryMenuGroup for: Package asCalypsoItemContext
]

{ #category : 'activation' }
ClyOpenCriticBrowserCommand >> defaultMenuItemName [
	^'Browse critiques'
]

{ #category : 'execution' }
ClyOpenCriticBrowserCommand >> execute [
	| env |

	env := RBPackageEnvironment new packages: packages.
	StCritiqueRuleSelectorPresenter openWithEnvironment: env removeTestCase: false
]
