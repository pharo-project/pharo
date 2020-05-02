"
When setting up a glamorous presentation it is recommended to pass blocks (or literals) into the setup methods (#title:, #display:,  #when: ɩ instead of performing an evaluation right away.

This affects the performance of inspector.
"
Class {
	#name : #ReInspectorPresentationsNonBlockSetupRule,
	#superclass : #ReAbstractRule,
	#category : #'Renraku-Rules'
}

{ #category : #'testing-interest' }
ReInspectorPresentationsNonBlockSetupRule class >> checksMethod [
	^ true
]

{ #category : #running }
ReInspectorPresentationsNonBlockSetupRule >> check: aMethod forCritiquesDo: aCriticBlock [

	(aMethod hasPragmaNamed: #gtInspectorPresentationOrder:) ifFalse: [ ^ self ].
	
	^ (aMethod ast sendNodes
			reject: [ :message |
				(self presentationScriptingMathods includes: message selector) ==> [
				| argument |
				argument := message arguments first.
				argument isBlock or: [ argument isLiteralNode ] ] ]
			thenCollect: [ :message |
				aCriticBlock cull: (ReTrivialCritique
					withAnchor: (ReIntervalSourceAnchor
						entity: aMethod
						interval: message	 sourceInterval)
					by: self) ])
				

]

{ #category : #accessing }
ReInspectorPresentationsNonBlockSetupRule >> group [
	^ 'Optimization'
]

{ #category : #accessing }
ReInspectorPresentationsNonBlockSetupRule >> name [

	^ 'Use blocks in scripting instead of plain messages'
]

{ #category : #private }
ReInspectorPresentationsNonBlockSetupRule >> presentationScriptingMathods [

	^ #(when: title: display: icon: with: send: format: sorted: tags:)
]
