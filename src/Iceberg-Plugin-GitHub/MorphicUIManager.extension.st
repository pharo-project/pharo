Extension { #name : #MorphicUIManager }

{ #category : #'*Iceberg-Plugin-GitHub' }
MorphicUIManager >> inform: aStringOrText actionOnClick: aBlock [
	"Display a message for the user to read and then dismiss. When clicked, execute an action."

	(ProvideAnswerNotification signal: aStringOrText) ifNotNil: [ :answer | ^ true ].
	(GrowlMorph openWithLabel: 'Information' translated contents: aStringOrText)
		vanishDelay: 2 second;
		actionBlock: aBlock
]
