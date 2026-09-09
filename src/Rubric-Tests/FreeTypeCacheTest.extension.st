Extension { #name : 'FreeTypeCacheTest' }

{ #category : '*Rubric-Tests' }
FreeTypeCacheTest >> testGlyphAccessIsThreadSafe [
	"This test is weak because it depends in the rendering of Morphic and Rubrik"
	"This test breaks 95% of times"

	self timeLimit: 20 seconds.

	10 timesRepeat: [
		 | sem text canvases blocky |

		sem := Semaphore new.
		text := (String loremIpsum: 25*1024).
		FreeTypeCache current removeAll.
		canvases := OrderedCollection new.

		blocky := [  | canvas |
			canvas := FormCanvas extent: 1000@1000.
			canvases add: canvas.
			(RubScrolledTextMorph new)
					setText: text;
					font: StandardFonts codeFont;
					bounds: (0@0 corner: canvas form extent);
					fullDrawOn: canvas.
			sem signal ].

		blocky forkAt: 39.
		blocky forkAt: 39.
		blocky forkAt: 39.

		sem
			wait;
			wait;
			wait.

		self assert: (((canvases at:1) form bits = (canvases at:2) form bits) and: [ ((canvases at:2) form bits = (canvases at:3) form bits) ])
	]
]

{ #category : '*Rubric-Tests' }
FreeTypeCacheTest >> testGlyphAccessIsThreadSafeWhenAskingWidth [
	"This test is weak because it depends in the rendering of Morphic and Rubrik"
	"This test breaks 95% of times"

	self timeLimit: 20 seconds.

	10 timesRepeat: [
		 | sem text canvases blocky widthAsk |

		sem := Semaphore new.
		text := (String loremIpsum: 25*1024).
		FreeTypeCache current removeAll.
		canvases := OrderedCollection new.

		widthAsk := [
			1 to: text size do: [ :i | StandardFonts codeFont widthOf: (text at: i). i % 50 = 0 ifTrue: [ Processor yield ]].
			sem signal ].


		blocky := [  | canvas |
			canvas := FormCanvas extent: 1000@1000.
			canvases add: canvas.
			(RubScrolledTextMorph new)
					setText: text;
					font: StandardFonts codeFont;
					bounds: (0@0 corner: canvas form extent);
					fullDrawOn: canvas.
			sem signal ].

		blocky forkAt: 39.
		blocky forkAt: 39.
		blocky forkAt: 39.
		widthAsk forkAt: 39.

		sem
			wait;
			wait;
			wait;
			wait.

		self assert: (((canvases at:1) form bits = (canvases at:2) form bits) and: [ ((canvases at:2) form bits = (canvases at:3) form bits) ])
	]
]
