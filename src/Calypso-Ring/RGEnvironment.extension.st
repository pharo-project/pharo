Extension { #name : #RGEnvironment }

{ #category : #'*Calypso-Ring' }
RGEnvironment >> adoptForBrowser [
	| unresolved extensionsPackage |
	extensionsPackage := self ensurePackageNamed: '*Extensions*'.
	unresolved := packages reject: [ :each | each hasResolved: #name ].
	unresolved do: [ :each | 
		each classes 
			select: [ :eachClass | (eachClass hasResolved: #name) and: [ 
					eachClass extensions notEmpty or: [ eachClass classSide extensions notEmpty ] ] ] 
			thenDo: [ :eachClass | eachClass package: extensionsPackage] ].
	unresolved do: [ :each | self removePackage: each ].
]

{ #category : #'*Calypso-Ring' }
RGEnvironment >> associationsDo: aBlock [

	self bindings do: aBlock
]

{ #category : #'*Calypso-Ring' }
RGEnvironment >> at: aSymbol [

	^ self ask behaviorNamed: aSymbol
]

{ #category : #'*Calypso-Ring' }
RGEnvironment >> at: aString ifAbsent: aBlockClosure [ 
	| association |
	association := self bindingOf: aString.
	^ association 
		ifNotNil: [ association value ]
		ifNil: aBlockClosure
]

{ #category : #'*Calypso-Ring' }
RGEnvironment >> bindings [

	"TODO:"

	| dict |
	
	dict := Dictionary new.
	self globalVariablesDo: [ :each | 
		each name -> each ].
	
	^ dict.
]

{ #category : #'*Calypso-Ring' }
RGEnvironment >> browse [

	^self browseAs: self printString
]

{ #category : #'*Calypso-Ring' }
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

	^ browser open.
]

{ #category : #'*Calypso-Ring' }
RGEnvironment >> classNamed: aString [ 
	| association |
	association := self bindingOf: aString.
	^ association ifNotNil: [ association value ]
		ifNil: [ self error: 'Class "', aString, '" not found' ] 
]

{ #category : #'*Calypso-Ring' }
RGEnvironment >> classNames [

	^ self allClasses collect: [ :each | each name ].
]

{ #category : #'*Calypso-Ring' }
RGEnvironment >> defineClass: aString withController: anObject [ 
	^ self compileANewClassFrom: aString notifying: nil startingFrom: nil

]

{ #category : #'*Calypso-Ring' }
RGEnvironment >> nonClassNames [

	"TODO:"
	^ #()
]

{ #category : #'*Calypso-Ring' }
RGEnvironment >> systemDictionary [
	^ self
]
