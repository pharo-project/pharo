Class {
	#name : #StHaltCacheTest,
	#superclass : #StHaltAndBreakpointControlTest,
	#category : #'NewTools-Debugger-Breakpoints-Tools-Tests'
}

{ #category : #'testing - halts cache' }
StHaltCacheTest >> testAddingHaltToMethod [
	| nodeCacheForSimpleHalt originalNodeCacheSize |
	nodeCacheForSimpleHalt := cache
		nodeCacheForSelector: 'StDummyTestClassWithHalts>>#mHalt'.
	originalNodeCacheSize := nodeCacheForSimpleHalt size.
	self testClass compile: 'mHalt self halt. self halt'.
	nodeCacheForSimpleHalt := cache
		nodeCacheForSelector: 'StDummyTestClassWithHalts>>#mHalt'.
	self
		assert: nodeCacheForSimpleHalt size
		equals: originalNodeCacheSize + 1.
	(self testClass >> #mHalt) ast statements
		do: [ :node | self assert: (nodeCacheForSimpleHalt includes: node) ].
	self
		assert:
			(cache methodsWithHalts keys
				select:
					[ :methodSelector | methodSelector = 'StDummyTestClassWithHalts>>#mHalt' ])
				size
		equals: 1
]

{ #category : #'testing - cache initialization' }
StHaltCacheTest >> testCacheActivation [
	cache removeSubscriptions.
	cache := StHaltCache new.
	cache isActive: true.
	self assert: cache isActive.
	self assert: (SystemAnnouncer uniqueInstance hasSubscriber: cache) 
]

{ #category : #'testing - cache initialization' }
StHaltCacheTest >> testCacheDectivation [
	cache removeSubscriptions.
	cache := StHaltCache new.
	cache isActive: true.
	self assert: (SystemAnnouncer uniqueInstance hasSubscriber: cache).
	cache isActive: false.
	self deny: (SystemAnnouncer uniqueInstance hasSubscriber: cache) 
]

{ #category : #'testing - cache initialization' }
StHaltCacheTest >> testInitialActivation [
	cache := StHaltCache new.
	self deny: cache isActive.
	self deny: (SystemAnnouncer uniqueInstance hasSubscriber: cache) 
]

{ #category : #'testing - cache building' }
StHaltCacheTest >> testInitialCacheBuild [
	self assert: cache methodsWithHalts size equals: 6.
	(self testClass methods
		reject: [ :m | 
			#('StDummyTestClassWithHalts>>#mNoHalt' 'StDummyTestClassWithHalts>>#mRejectHalt')
				includes: m printString ])
		do: [ :method | 
			| haltNode nodeCache |
			haltNode := method ast statements first.
			self
				assert: (cache methodsWithHalts keys includes: method printString).
			nodeCache := cache nodeCacheForSelector: method printString.
			self assert: nodeCache size equals: 1.
			self
				assert: nodeCache asOrderedCollection first
				identicalTo: haltNode ]
]

{ #category : #'testing - halts cache' }
StHaltCacheTest >> testMethodWithHaltRemoval [
	|methodCacheForSimpleHalt |
	methodCacheForSimpleHalt := cache methodsWithHalts size.
	
	self testClass removeSelector: #mHalt.	
	self assert: cache methodsWithHalts size equals: methodCacheForSimpleHalt - 1.
	self assert: 
		(cache methodsWithHalts keys select:[:methodSelector| methodSelector = 'StDummyTestClassWithHalts>>#mHalt']) isEmpty
]

{ #category : #'testing - halts cache' }
StHaltCacheTest >> testNewMethodWithHalt [
	|methodCacheForSimpleHalt haltNode|
	methodCacheForSimpleHalt := cache methodsWithHalts size.
	
	self testClass compile: 'mHalt2 self halt.'.	
	haltNode := (self testClass>>#mHalt2) ast statements first.	
	
	self assert: cache methodsWithHalts size equals: methodCacheForSimpleHalt + 1.
	self assert: (cache nodeCacheForSelector: 'StDummyTestClassWithHalts>>#mHalt2') size equals: 1.
	self assert: (cache nodeCacheForSelector: 'StDummyTestClassWithHalts>>#mHalt2') asOrderedCollection first identicalTo: haltNode.

]

{ #category : #'testing - cache building' }
StHaltCacheTest >> testRefreshCacheWithScanAllSystem [
	|newCache|
	newCache := StHaltCache new.
	newCache scanAllSystem: true.
	newCache refreshCache.
	self assert: cache methodsWithHalts size >= 6
]

{ #category : #'testing - cache building' }
StHaltCacheTest >> testRefreshCacheWithoutScanAllSystem [
	|newCache|
	newCache := StHaltCache new.
	newCache refreshCache.
	self assert: newCache methodsWithHalts isEmpty
]

{ #category : #'testing - halts cache' }
StHaltCacheTest >> testRemoveAllHaltsFromMethod [
	| methodCacheForSimpleHalt |
	methodCacheForSimpleHalt := cache methodsWithHalts size.
	self testClass compile: 'mHalt ^self'.
	self
		assert: cache methodsWithHalts size
		equals: methodCacheForSimpleHalt - 1.
	self
		assert:
			(cache methodsWithHalts keys
				select:
					[ :methodSelector | methodSelector = 'StDummyTestClassWithHalts>>#mHalt' ])
				isEmpty
]

{ #category : #'testing - cache building' }
StHaltCacheTest >> testScanAllSystemDisabled [
	|newCache|
	newCache := StHaltCache new.
	self assert: newCache methodsWithHalts isEmpty
]

{ #category : #'testing - cache building' }
StHaltCacheTest >> testScanAllSystemEnabled [
	|newCache|
	newCache := StHaltCache basicNew.
	newCache scanAllSystem: true.
	newCache initialize.
	self assert: cache methodsWithHalts size >= 6
]

{ #category : #'testing - halts cache' }
StHaltCacheTest >> testShouldConsiderHaltNode [
	| node |
	node := RBMessageNode receiver: RBVariableNode selfNode selector: #haltOnAccessTo.
	self deny: (cache shouldConsiderHaltNode: node).
		
	node := RBMessageNode receiver: RBVariableNode selfNode selector: #haltOnRead.
	self deny: (cache shouldConsiderHaltNode: node).
	
	node := RBMessageNode receiver: RBVariableNode selfNode selector: #haltOnWrite.
	self deny: (cache shouldConsiderHaltNode: node).
	
	node := RBMessageNode receiver: RBVariableNode selfNode selector: #haltOnAccess.
	self deny: (cache shouldConsiderHaltNode: node).
	
	node := RBMessageNode receiver: RBVariableNode selfNode selector: #mhalt.
	self deny: (cache shouldConsiderHaltNode: node).
	
	node := RBMessageNode receiver: RBVariableNode selfNode selector: #halt.
	self assert: (cache shouldConsiderHaltNode: node).
	
	node := RBMessageNode receiver: RBVariableNode selfNode selector: #haltOnce.
	self assert: (cache shouldConsiderHaltNode: node).
	
	node := RBMessageNode receiver: RBVariableNode selfNode selector: #haltIf.
	self assert: (cache shouldConsiderHaltNode: node).
	
	node := RBMessageNode receiver: RBVariableNode selfNode selector: #haltOnCallTo.
	self assert: (cache shouldConsiderHaltNode: node).
	
	
]
