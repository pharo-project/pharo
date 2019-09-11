Extension { #name : #ClyBrowserMorph }

{ #category : #'*Calypso-SystemTools-Core' }
ClyBrowserMorph >> allowSilentlyRemoveMethods: methods whichHaveSenders: senders [

	| singleSender sendersCount |
	sendersCount := senders size.
	sendersCount = 0 ifTrue: [ ^true].
	methods size = 1 & (sendersCount = 1) ifFalse: [^false].
	 
	singleSender := senders itemAt: 1.
	^singleSender == methods first
]

{ #category : #'*Calypso-SystemTools-Core' }
ClyBrowserMorph >> confirmEmptySystemQuery: aQuery [
	
	| showResult result |
	
	result := aQuery execute.
	result isEmpty ifTrue: [ ^true ].
	
	showResult := UIManager default confirm: 'There are '
		, result size asString, ' ', aQuery description, '. Show them?'.
	showResult ifTrue: [self spawnQueryBrowserOn: aQuery].
	^false
]

{ #category : #'*Calypso-SystemTools-Core' }
ClyBrowserMorph >> confirmEmptySystemQuery: aQuery excluding: classes [
	
	| showResult result |
	
	result := aQuery execute.
	(result isEmpty or: [ ((result items collect: [:each | each origin]) difference: classes ) isEmpty]) ifTrue: [ ^true ].
	
	showResult := UIManager default confirm: 'There are '
		, result size asString, ' ', aQuery description, '. Show them?'.
	showResult ifTrue: [self spawnQueryBrowserOn: aQuery].
	^false
]

{ #category : #'*Calypso-SystemTools-Core' }
ClyBrowserMorph >> confirmUnusedClasses: classes [
	
	| refQuery noUsers answer subclasses users |
	
	refQuery := ClyClassReferencesQuery toAny: classes from: self systemScope.	
	"we need to exclude both the class and instance side of the class"
	noUsers := self confirmEmptySystemQuery: refQuery excluding: (classes flatCollect: [ :each | { each . each classSide } ]).
	
	subclasses := (classes flatCollect: [:each | each subclasses]) copyWithoutAll: classes.
	subclasses ifNotEmpty: [ 
		noUsers := false.
		answer := UIManager default confirm: 'There are subclasses. Show them?'.
		answer ifTrue: [ self spawnQueryBrowserOn: (ClyConstantQuery returning: subclasses) ] ].
	
	users := (classes flatCollect: [:each | each users]) copyWithoutAll: classes.
	users ifNotEmpty: [ 
		noUsers := false.
		answer := UIManager default confirm: 'There are users of trait. Show them?'. 
		answer ifTrue: [ self spawnQueryBrowserOn: (ClyConstantQuery returning: users) ]].
	
	^noUsers
]

{ #category : #'*Calypso-SystemTools-Core' }
ClyBrowserMorph >> confirmUnusedVariables: variables [
	
	| refQuery |
	refQuery := ClyVariableReferencesQuery ofAny: variables from: self systemScope.
	
	^self confirmEmptySystemQuery: refQuery
]

{ #category : #'*Calypso-SystemTools-Core' }
ClyBrowserMorph >> confirmUnusedVariablesInDefiningClass: variables [
	
	| refQuery classScope |
	classScope := ClyBothMetaLevelClassScope ofAll: (variables collect: [:each | each definingClass]) in: navigationEnvironment. 
	refQuery := ClyVariableReferencesQuery ofAny: variables from: classScope.
	
	^self confirmEmptySystemQuery: refQuery
]

{ #category : #'*Calypso-SystemTools-Core' }
ClyBrowserMorph >> decorateMethodEditor: aMethodEditor [
]

{ #category : #'*Calypso-SystemTools-Core' }
ClyBrowserMorph >> isMethodSelected: aMethod [

	^self methodSelection includesActualObject: aMethod
]

{ #category : #'*Calypso-SystemTools-Core' }
ClyBrowserMorph >> requestRemoveMethodStrategyFor: methods [
	| selectors result strategies caption  senders senderCount plural |
	
	selectors := (methods collect: [ :each | each selector ] as: IdentitySet) asArray.	
	
	senders := (ClyMessageSendersQuery ofAny: selectors from: self systemScope) execute.
	(self allowSilentlyRemoveMethods: methods whichHaveSenders: senders)
		ifTrue: [ ^SycSilentlyRemoveMethodStrategy new ].

	strategies := SycRemoveMethodStrategy createAllForBrowser: self.
	senderCount := senders size.
	plural := '' asPluralBasedOn: senderCount.
	
	caption := selectors size > 1 
		ifTrue: [ 'There are {1} sender{2}' format: {senderCount. plural}]
		ifFalse: ['The message #{1} has {2} sender{3}' format: {selectors anyOne. senderCount. plural}].	
			
	result := UIManager default 
		chooseFrom: (strategies collect: #userRequestString) values: strategies title: caption.
		
	^result ifNil: [ SycNotRemoveMethodStrategy new ]
]
