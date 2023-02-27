Class {
	#name : #AthensCairoCanvasTest,
	#superclass : #TestCase,
	#category : #'Athens-Cairo-Tests-Library'
}

{ #category : #tests }
AthensCairoCanvasTest >> testTextExtentsAnswerValidData [
	"This works almost as a smoke test since the objective is validate image does not
	 crash, but it will try to valid it also answers something reasonable"
	| surface cairoExtents |

	surface := AthensCairoSurface extent: 100@100.
	surface drawDuring: [ :aCanvas |
		cairoExtents := CairoFontExtents new.
		self assert: cairoExtents width equals: 0.
		aCanvas text: 'Test' extents: cairoExtents.
		self deny: cairoExtents width equals: 0	]
]
