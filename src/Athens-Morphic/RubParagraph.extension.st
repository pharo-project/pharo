Extension { #name : 'RubParagraph' }

{ #category : '*Athens-Morphic' }
RubParagraph >> displayTextOnAthensCanvas: anAthensCanvas in: aRectangle [
	| athensDisplayScanner offset leftInRun |
	anAthensCanvas clipBy: aRectangle during:[
	anAthensCanvas pathTransform
		restoreAfter: [
			offset := aRectangle topLeft - self position.
			athensDisplayScanner := AthensDisplayScanner for: self on: anAthensCanvas.
			leftInRun := 0.
			self
				forLinesIn: anAthensCanvas clipRect
				do: [ :line |
					line first <= line last
						ifTrue: [ leftInRun := athensDisplayScanner displayLine: line offset: offset leftInRun: leftInRun ] ] ] ]
]

{ #category : '*Athens-Morphic' }
RubParagraph >> drawOnAthensCanvas: aCanvas bounds: aRectangle color: aColor [
	aCanvas setPaint: aColor.
	self displayTextOnAthensCanvas: aCanvas in: aRectangle
]
