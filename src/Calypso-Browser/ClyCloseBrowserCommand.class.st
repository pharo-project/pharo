"
I am a command to close browser window. 

There are already system shortcuts like ""$w command"" to close current window. But some morphs like tab manager are implementing it differently.
In a browser the desired behaviour is to close the full window instead of single tab. 
I override it by shortcut activator for source code context. 
When you edit code and press ""$w command"" I will close browser window. 
Also it fixes the case for method browser managed by tabs group window. 
Before ""$w command"", it keeps an empty tab without closing it.
"
Class {
	#name : #ClyCloseBrowserCommand,
	#superclass : #ClyBrowserCommand,
	#category : #'Calypso-Browser-Commands'
}

{ #category : #activation }
ClyCloseBrowserCommand class >> browserShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation by: $w meta for: ClyBrowserContext
]

{ #category : #activation }
ClyCloseBrowserCommand class >> textToolShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation by: $w meta for: ClyTextEditorContext
]

{ #category : #execution }
ClyCloseBrowserCommand >> execute [
	browser close
]
