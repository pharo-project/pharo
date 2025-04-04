Extension { #name : 'ClyBrowserMorph' }

{ #category : '*Calypso-SystemTools-QueryBrowser' }
ClyBrowserMorph >> browseImplementorsOf: aSymbol inNameResolver: aNameResolver [
	| classBinding classToBrowse |
	aSymbol first isUppercase ifTrue: [
		aNameResolver 
			ifNil: [ classToBrowse := self class environment at: aSymbol.
						^ { ClyFullBrowserMorph openOnClass: classToBrowse.
						#BrowseClassWithoutNameResolver} ]
			ifNotNil: [ 
		classBinding := aNameResolver bindingOf: aSymbol.
	   
		classBinding ifNotNil: [
			classToBrowse := classBinding definingClass ifNil: [ classBinding value ].
			classToBrowse isClassOrTrait ifFalse: [ classToBrowse := classToBrowse class ].
			self spawnBrowser: ClyFullBrowserMorph withState: [ :browser |
				browser selectClass: classToBrowse].
			^ #NormalBrowseClass ] ] ].

	((Smalltalk tools toolNamed: #messageList)
		browseImplementorsOf: aSymbol
		inScope: self defaultNavigationScope asRBEnvironment ).
	^ #NormalImplementorBrowse
]

{ #category : '*Calypso-SystemTools-QueryBrowser' }
ClyBrowserMorph >> browseLowercasedReferencesTo: aSymbol inNameResolver: anEnvironment [

	^ {
		(self browseSendersOf: aSymbol).
		#SendersNoIdeaWhenSent 
		}
]

{ #category : '*Calypso-SystemTools-QueryBrowser' }
ClyBrowserMorph >> browseReferencesTo: aSymbol [

	self browseReferencesTo: aSymbol inNameResolver: self system
]

{ #category : '*Calypso-SystemTools-QueryBrowser' }
ClyBrowserMorph >> browseReferencesTo: aSymbol inNameResolver: anEnvironment [

	aSymbol isSymbol and: [
			^ aSymbol first isUppercase
				  ifTrue: [
						  self
							  browseUppercasedReferencesTo: aSymbol
							  inNameRespolver: anEnvironment ]
				  ifFalse: [
						  (Smalltalk tools toolNamed: #messageList)
							  browseSendersOf: aSymbol
							  inScope: self defaultNavigationScope asRBEnvironment ] ]
]

{ #category : '*Calypso-SystemTools-QueryBrowser' }
ClyBrowserMorph >> browseSendersOf: aSymbol [

	self spawnQueryBrowserOn: (ClyMessageSendersQuery of: aSymbol)
]

{ #category : '*Calypso-SystemTools-QueryBrowser' }
ClyBrowserMorph >> browseUppercasedReferencesTo: aSymbol inNameRespolver: anEnvironment [

	| aClassOrTrait returnedSymbol |
	
	aClassOrTrait := anEnvironment 
		ifNil: [ returnedSymbol := #SendersWithouEnvironment. self class environment at: aSymbol ] 
		ifNotNil: [ returnedSymbol := #SendersWithEnvironment. (anEnvironment bindingOf: aSymbol) value ].

	^ { SystemNavigation default browseAllUsersOfClassOrTrait: aClassOrTrait. returnedSymbol }
]

{ #category : '*Calypso-SystemTools-QueryBrowser' }
ClyBrowserMorph >> spawnQueryBrowserOn: aQuery [
	
	self spawnQueryBrowserOn: aQuery withState: []
]

{ #category : '*Calypso-SystemTools-QueryBrowser' }
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

	self 
		spawnBrowser: (Smalltalk tools toolNamed: #messageList)
		withState: [ :browser |
			browser queryScopes: self allNavigationScopes.
			browser showResultOf: targetQuery.
			navigationBlock valueWithPossibleArgs: {browser} ]
]
