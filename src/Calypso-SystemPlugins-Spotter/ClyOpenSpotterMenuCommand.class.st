"
I'm a command to open ""spotter navigation"" menu.
This navigation brings an easy way to access all available panels and commands of selected browser.

activation is usually cmd+/ (because is close to the arrows)
"
Class {
	#name : #ClyOpenSpotterMenuCommand,
	#superclass : #ClyBrowserCommand,
	#category : #'Calypso-SystemPlugins-Spotter'
}

{ #category : #activation }
ClyOpenSpotterMenuCommand class >> browserShortcutActivation [
	<classAnnotation>

	^ CmdShortcutActivation by: $h meta for: ClyBrowserContext
]

{ #category : #accessing }
ClyOpenSpotterMenuCommand class >> defaultMenuIconName [

	^#smallFind
]

{ #category : #accessing }
ClyOpenSpotterMenuCommand class >> defaultMenuItemName [

	^ String streamContents: [ :stream |
		stream 
			<< 'Open context spotter (' 
			<< self textToolShortcutActivation keyCombination asString 
			<< ')' ]
]

{ #category : #activation }
ClyOpenSpotterMenuCommand class >> methodBrowserTabActivation [
	<classAnnotation>
	
	^ ClyBrowserTabCommandActivation for: ClyMethod asCalypsoItemContext 
]

{ #category : #activation }
ClyOpenSpotterMenuCommand class >> textToolShortcutActivation [
	<classAnnotation>

	^ CmdShortcutActivation by: $h meta for: ClyTextEditorContext
]

{ #category : #execution }
ClyOpenSpotterMenuCommand >> execute [
	GTSpotterMorph new
		extent: (World width / 2.4 @ (World height / 1.6)) asIntegerPoint;
		doLayout;
		spotterModel: (GTSpotter on: (ClySpotterModel on: browser));
		openCenteredInWorld	
]
