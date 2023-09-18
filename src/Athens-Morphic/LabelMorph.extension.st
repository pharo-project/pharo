Extension { #name : #LabelMorph }

{ #category : #'*Athens-Morphic' }
LabelMorph >> drawOnAthensCanvas: aCanvas [
	"Draw based on enablement."

	| pc |
	pc := self owner
		ifNil: [ self paneColor ]
		ifNotNil: [
			self owner color isTransparent
				ifTrue: [ self owner paneColor ]
				ifFalse: [ self owner color ] ].
	aCanvas
		clipBy: self bounds
		during: [
			aCanvas pathTransform
				restoreAfter: [
					aCanvas pathTransform translateX: self left Y: self top + self fontToUse ascent.
					aCanvas
						setFont: self fontToUse;
						setPaint:
								(self enabled
										ifTrue: [ self color ]
										ifFalse: [ pc muchDarker ]);
						drawString: self contents ] ]
]
