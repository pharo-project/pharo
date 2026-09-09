Extension { #name : 'RGEnvironment' }

{ #category : '*Calypso-Ring' }
RGEnvironment >> adoptForBrowser [
	| unresolved extensionsPackage |
	extensionsPackage := self ensurePackageNamed: '*Extensions*'.
	unresolved := packages reject: [ :each | each hasResolved: #name ].
	unresolved do: [ :each |
		each classes
			select: [ :eachClass | (eachClass hasResolved: #name) and: [
					eachClass extensions notEmpty or: [ eachClass classSide extensions notEmpty ] ] ]
			thenDo: [ :eachClass | eachClass package: extensionsPackage] ].
	unresolved do: [ :each | self removePackage: each ]
]

{ #category : '*Calypso-Ring' }
RGEnvironment >> browse [

	^self browseAs: self printString
]

{ #category : '*Calypso-Ring' }
RGEnvironment >> browseAs: browserTitleString [

	 | clyEnv clyNavEnv browser |
	self adoptForBrowser.
	clyEnv := ClyRing2Environment new.
	clyEnv environment: self.
	clyEnv name: browserTitleString.

	clyNavEnv := (ClyNavigationEnvironment over: clyEnv)
		addPlugin: ClyDefaultSystemEnvironmentPlugin new;
	   attachToSystem.

	browser := ClyFullBrowserMorph new
		navigationEnvironment: clyNavEnv;
		addPlugin: ClyStandardBrowserPlugin new.

	^ browser open
]
