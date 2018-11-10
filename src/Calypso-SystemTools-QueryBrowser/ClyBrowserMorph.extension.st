Extension { #name : #ClyBrowserMorph }

{ #category : #'*Calypso-SystemTools-QueryBrowser' }
ClyBrowserMorph >> browseImplementorsOf: aSymbol [
	| classBinding classToBrowse |
	aSymbol first isUppercase ifTrue: [ 
		classBinding := self system bindingOf: aSymbol.
		classBinding ifNotNil: [ 
			classToBrowse := classBinding value.
			classToBrowse isClassOrTrait ifFalse: [ classToBrowse := classToBrowse class ].
			^self spawnBrowser: ClyFullBrowser withState: [ :browser | 
				browser selectClass: classToBrowse]]].
	
	self spawnQueryBrowserOn: (ClyMessageImplementors of: aSymbol)
]

{ #category : #'*Calypso-SystemTools-QueryBrowser' }
ClyBrowserMorph >> browseReferencesTo: aSymbol [
	
	self browseReferencesTo: aSymbol from: self system
]

{ #category : #'*Calypso-SystemTools-QueryBrowser' }
ClyBrowserMorph >> browseReferencesTo: aSymbol from: anEnvironment [
	
	| classBinding |
	aSymbol first isUppercase ifTrue: [ 
		classBinding := anEnvironment bindingOf: aSymbol.
		classBinding ifNotNil: [ 
			^self spawnQueryBrowserOn: (ClyClassReferences of: classBinding)]].
		
	self browseSendersOf: aSymbol
]

{ #category : #'*Calypso-SystemTools-QueryBrowser' }
ClyBrowserMorph >> browseSendersOf: aSymbol [
	
	self spawnQueryBrowserOn: (ClyMessageSenders of: aSymbol)
]

{ #category : #'*Calypso-SystemTools-QueryBrowser' }
ClyBrowserMorph >> spawnQueryBrowserOn: aQuery [
	
	^self spawnQueryBrowserOn: aQuery withState: []
]

{ #category : #'*Calypso-SystemTools-QueryBrowser' }
ClyBrowserMorph >> spawnQueryBrowserOn: aQuery withState: navigationBlock [
	| targetQuery |
	targetQuery := aQuery.
	aQuery isBoundToEnvironment ifFalse: [ 
		targetQuery := aQuery withScope: self defaultNavigationScope].
	
	^self spawnBrowser: ClyQueryBrowser withState: [ :browser | 
		browser queryScopes: self allNavigationScopes.
		browser showResultOf: targetQuery.
		navigationBlock valueWithPossibleArgs: {browser}]
]
