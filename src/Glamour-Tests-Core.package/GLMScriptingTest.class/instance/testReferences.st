tests
testReferences
	| browser pane1 |
	browser := GLMExplicitBrowser new.
	pane1 := browser addNewPaneNamed: #pane1.
	self assert: (browser resolveOriginPort: #pane1) == (pane1 port: #selection).
	self assert: (browser resolveOriginPort: #pane1 -> #foo) == (pane1 port: #foo).
	self assert: (browser resolveDestinationPort: #pane1) == (pane1 port: #entity).
	self assert: (browser resolveDestinationPort: #pane1 -> #foo) == (pane1 port: #foo).
	self assert: ((browser resolveOriginPort: #outer) isKindOf: GLMPresentationBoundPort).
	self assert: (browser resolveOriginPort: #outer) presentation == browser.
	self assert: (browser resolveOriginPort: #outer) name equals: #selection.
	self assert: ((browser resolveOriginPort: #outer -> #foo) isKindOf: GLMPresentationBoundPort).
	self assert: (browser resolveOriginPort: #outer -> #foo) presentation == browser.
	self assert: (browser resolveOriginPort: #outer -> #foo) name equals: #foo.
	self assert: ((browser resolveDestinationPort: #outer) isKindOf: GLMPresentationBoundPort).
	self assert: (browser resolveDestinationPort: #outer) presentation == browser.
	self assert: (browser resolveDestinationPort: #outer) name equals: #entity.
	self assert: ((browser resolveDestinationPort: #outer -> #foo) isKindOf: GLMPresentationBoundPort).
	self assert: (browser resolveDestinationPort: #outer -> #foo) presentation == browser.
	self assert: (browser resolveDestinationPort: #outer -> #foo) name equals: #foo