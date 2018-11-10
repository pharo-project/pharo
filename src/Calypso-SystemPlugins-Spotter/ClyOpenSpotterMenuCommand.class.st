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

	^ CmdShortcutActivation by: $/ meta for: ClyBrowserContext
]

{ #category : #activation }
ClyOpenSpotterMenuCommand class >> textToolShortcutActivation [
	<classAnnotation>

	^ CmdShortcutActivation by: $/ meta for: ClyTextEditorContext
]

{ #category : #execution }
ClyOpenSpotterMenuCommand >> execute [
	GTSpotterMorph new
		extent: (World width / 2.4 @ (World height / 1.6)) asIntegerPoint;
		doLayout;
		spotterModel: (GTSpotter on: (ClySpotterModel on: browser));
		openCenteredInWorld	
]
