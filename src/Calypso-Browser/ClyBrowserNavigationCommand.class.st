"
I am a root of navigation command classes.

I just define common places (activations) where commands should be accessible.
By default shorcuts are alt+left and alt+right to go back and forward.
"
Class {
	#name : #ClyBrowserNavigationCommand,
	#superclass : #ClyBrowserCommand,
	#category : #'Calypso-Browser-NavigationHistory'
}

{ #category : #activation }
ClyBrowserNavigationCommand class >> browserShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation by: self defaultShortcut for: ClyBrowserContext
]

{ #category : #activation }
ClyBrowserNavigationCommand class >> browserTabActivation [
	<classAnnotation>
	
	^ClyBrowserTabCommandActivation for: ClyBrowserContext
]

{ #category : #activation }
ClyBrowserNavigationCommand class >> defaultShortcut [
	<classAnnotationDependency>
	
	self subclassResponsibility 
]

{ #category : #testing }
ClyBrowserNavigationCommand class >> isAbstract [
	^self = ClyBrowserNavigationCommand
]

{ #category : #activation }
ClyBrowserNavigationCommand class >> textEditorShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation by: self defaultShortcut for: ClyTextEditorContext
]
