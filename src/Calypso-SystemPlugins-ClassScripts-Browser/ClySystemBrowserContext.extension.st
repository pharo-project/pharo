Extension { #name : #ClySystemBrowserContext }

{ #category : #'*Calypso-SystemPlugins-ClassScripts-Browser' }
ClySystemBrowserContext >> isClassScriptSelected [

	^self isMethodSelected and: [ 
		(self lastSelectedObjectIn: self selectedMethodItems) hasProperty: ClyClassScriptProperty]
]

{ #category : #'*Calypso-SystemPlugins-ClassScripts-Browser' }
ClySystemBrowserContext >> selectedClassScript [

	| methodItem |
	methodItem := self lastSelectedObjectIn: self selectedMethodItems.
	^(methodItem getProperty: ClyClassScriptProperty) script
]
