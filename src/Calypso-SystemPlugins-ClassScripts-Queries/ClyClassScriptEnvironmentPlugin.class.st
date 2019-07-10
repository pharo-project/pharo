"
I am special plugin which decorates scripting class side methods.
I use hierarchy of ClyClassScript to detect that given method provides a script
"
Class {
	#name : #ClyClassScriptEnvironmentPlugin,
	#superclass : #ClySystemEnvironmentPlugin,
	#category : #'Calypso-SystemPlugins-ClassScripts-Queries'
}

{ #category : #'item decoration' }
ClyClassScriptEnvironmentPlugin >> decorateBrowserItem: anItem ofMethod: aMethod [

	| script |
	script := ClyClassScript detectInMethod: aMethod ifAbsent: [ ^ self ].
	anItem addProperty: (ClyClassScriptProperty script: script)
]
