"
I am a global context of full browser
"
Class {
	#name : 'ClyFullBrowserGlobalContext',
	#superclass : 'ClyFullBrowserContext',
	#category : 'Calypso-SystemTools-FullBrowser-Contexts',
	#package : 'Calypso-SystemTools-FullBrowser',
	#tag : 'Contexts'
}

{ #category : 'commands' }
ClyFullBrowserGlobalContext class >> calypsoSystemToolsFullBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap shortcuts
		add: ClyFindClassCommand key: $f meta;
		add: SycAddNewPackageCommand key: $k meta , $p meta
]
