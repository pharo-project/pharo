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
	
	self spawnQueryBrowserOn: (ClyMessageImplementorsQuery of: aSymbol)
]

{ #category : #'*Calypso-SystemTools-QueryBrowser' }
ClyBrowserMorph >> browseReferencesTo: aSymbol [
	
	self browseReferencesTo: aSymbol from: self system
]

{ #category : #'*Calypso-SystemTools-QueryBrowser' }
ClyBrowserMorph >> browseReferencesTo: aSymbol from: anEnvironment [
	
	| classBinding |
	aSymbol isSymbol and: [ aSymbol first isUppercase ifTrue: [ 
		classBinding := anEnvironment bindingOf: aSymbol.
		classBinding ifNotNil: [ 
			^self spawnQueryBrowserOn: (ClyClassReferencesQuery of: classBinding)]]].
		
	self browseSendersOf: aSymbol
]

{ #category : #'*Calypso-SystemTools-QueryBrowser' }
ClyBrowserMorph >> browseSendersOf: aSymbol [
	
	self spawnQueryBrowserOn: (ClyMessageSendersQuery of: aSymbol)
]

{ #category : #'*Calypso-SystemTools-QueryBrowser' }
ClyBrowserMorph >> spawnQueryBrowserOn: aQuery [
	
	self spawnQueryBrowserOn: aQuery withState: []
]

{ #category : #'*Calypso-SystemTools-QueryBrowser' }
ClyBrowserMorph >> spawnQueryBrowserOn: aQuery withState: navigationBlock [
	| targetQuery |
	targetQuery := aQuery.
	aQuery isBoundToEnvironment ifFalse: [ 
		targetQuery := aQuery withScope: self defaultNavigationScope].
	
	targetQuery scope = self systemScope ifTrue: [ 
		"If query is empty in system (global) scope then it is empty in any other scope.
		In that case we do not need a query browser because it will be always empty and useless"
		targetQuery semiAsync hasEmptyResult ifTrue: [ 
			"For slow queries we will not wait and open a browser to indicate execution progress"
			^self inform: 'There are no ', targetQuery description ]  ].
	
	self spawnBrowser: ClyQueryBrowser withState: [ :browser | 
		browser queryScopes: self allNavigationScopes.
		browser showResultOf: targetQuery.
		navigationBlock valueWithPossibleArgs: {browser}]
]
