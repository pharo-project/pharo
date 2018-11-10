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
ClyBrowserMorph >> confirmVariableRemoveFor: variables [
	
	| refQuery refs showRefs |
	refQuery := ClyVariableReferences ofAny: variables from: self systemScope.
	
	refs := refQuery execute.
	refs isEmpty ifTrue: [ ^true ].
	
	showRefs := UIManager default confirm: 'There are '
		, refs size asString, ' ', refQuery description, '. Show them?'.
	showRefs ifTrue: [self spawnQueryBrowserOn: refQuery].
	^false
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
	| selectors result strategies caption  senders |
	selectors := methods collect: [ :each | each selector ] as: IdentitySet.	
	selectors := selectors asArray.
	senders := (ClyMessageSenders ofAny: selectors from: self systemScope) execute.
	(self allowSilentlyRemoveMethods: methods whichHaveSenders: senders)
		ifTrue: [ ^SycSilentlyRemoveMethodStrategy new ].

	strategies := SycRemoveMethodStrategy createAllForBrowser: self.
	caption := selectors size > 1 
		ifTrue: [ 'There are ', senders size asString, ' senders']
		ifFalse: ['The message ', selectors anyOne printString ,' has ' , senders size asString , ' sender'].	
	result := UIManager default 
		chooseFrom: (strategies collect: #userRequestString) values: strategies title: caption.
	^result ifNil: [ SycNotRemoveMethodStrategy new ]
]
