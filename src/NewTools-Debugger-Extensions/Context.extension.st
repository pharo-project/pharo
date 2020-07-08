Extension { #name : #Context }

{ #category : #'*NewTools-Debugger-Extensions' }
Context >> debuggerInspectorTempsNodes [
	
	^  (StNodeCollector for: self) collectTempsNodes
]

{ #category : #'*NewTools-Debugger-Extensions' }
Context >> findContextSuchThat: testBlock depth: depth [
	"Search self and my sender chain for first one that satisfies testBlock.  Return nil if none satisfy.
	I limit the search to the depth first senders of the sender chain."

	| context counter |
	context := self.
	counter := 0.
	[ context isNil or: [ counter = depth ] ] whileFalse: [ 
		(testBlock value: context) ifTrue: [ ^ context ].
		context := context sender.
		counter := counter + 1 ].
	^ nil
]
