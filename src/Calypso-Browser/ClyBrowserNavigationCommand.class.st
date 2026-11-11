"
I am a root of navigation command classes.

I just define common places (activations) where commands should be accessible.
By default shorcuts are alt+left and alt+right to go back and forward.
"
Class {
	#name : 'ClyBrowserNavigationCommand',
	#superclass : 'ClyBrowserCommand',
	#category : 'Calypso-Browser-NavigationHistory',
	#package : 'Calypso-Browser',
	#tag : 'NavigationHistory'
}

{ #category : 'activation' }
ClyBrowserNavigationCommand class >> defaultShortcut [
	<classAnnotationDependency>

	self subclassResponsibility
]

{ #category : 'testing' }
ClyBrowserNavigationCommand class >> isAbstract [
	^self = ClyBrowserNavigationCommand
]
