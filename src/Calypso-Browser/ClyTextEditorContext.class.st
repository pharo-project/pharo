"
I am default text editor context
"
Class {
	#name : 'ClyTextEditorContext',
	#superclass : 'ClyToolContext',
	#category : 'Calypso-Browser-TextEditors',
	#package : 'Calypso-Browser',
	#tag : 'TextEditors'
}

{ #category : 'commands' }
ClyTextEditorContext class >> calypsoBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap shortcuts
		add: ClyCloseBrowserCommand key: $w meta;
		add: ClyNavigateBrowserBackCommand key: Character arrowLeft alt;
		add: ClyNavigateBrowserForwardCommand key: Character arrowRight alt
]

{ #category : 'public' }
ClyTextEditorContext >> browser [
	^tool browser
]

{ #category : 'public' }
ClyTextEditorContext >> editor [
	^ tool
]

{ #category : 'public' }
ClyTextEditorContext >> selectedTextInterval [
	^tool selectedTextInterval
]
