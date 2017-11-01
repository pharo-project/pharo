"
If you execute some code during the method execution it will  slow down the inspector.

Try to put all the code in blocks in the setup messages, such as #title:, #display:,  #when:. Buy doing this your code will be executed lazily when the presentation is selected.
"
Class {
	#name : #ReInspectorPresentationsNonlazyCode,
	#superclass : #ReAbstractRule,
	#category : 'Renraku-Rules'
}

{ #category : #testing-interest }
ReInspectorPresentationsNonlazyCode class >> checksMethod [
	^ true
]

{ #category : #running }
ReInspectorPresentationsNonlazyCode >> check: aMethod forCritiquesDo: aCriticBlock [

	(aMethod hasPragmaNamed: #gtInspectorPresentationOrder:) ifFalse: [ ^ self ].
	
	^ (aMethod ast statements
			reject: [ :statement |
				self
					presentation: aMethod ast arguments first
					isTheLeftmostRecepientIn: (statement isReturn
						ifTrue: [ statement value ]
						ifFalse: [ statement ]) ]			
			thenCollect: [ :statement |
				aCriticBlock cull: (ReTrivialCritique
					withAnchor: (ReIntervalSourceAnchor
						entity: aMethod
						interval: statement sourceInterval)
					by: self) ])
]

{ #category : #accessing }
ReInspectorPresentationsNonlazyCode >> group [
	^ 'Optimization'
]

{ #category : #accessing }
ReInspectorPresentationsNonlazyCode >> name [

	^ 'Code executed outside of the presentation setup'
]

{ #category : #private }
ReInspectorPresentationsNonlazyCode >> presentation: aPresentation isTheLeftmostRecepientIn: aStatement [

	^ (aStatement isMessage or:
	 [ aStatement isCascade ])  and: [ 
			aStatement leftmostChainReceiver = aPresentation ]
]
