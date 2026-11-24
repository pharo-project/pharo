Extension { #name : 'PragmaMenuBuilder' }

{ #category : '*Morphic-Widgets' }
PragmaMenuBuilder >> fallbackMenu [

	^ FallbackMenu when: self fails: self model
]

{ #category : '*Morphic-Widgets' }
PragmaMenuBuilder >> menu [
	"returns a MenuMorph build from my menuSpec"

	^ self menuFrom: [ self menuSpec ]
]

{ #category : '*Morphic-Widgets' }
PragmaMenuBuilder >> menuAt: aName [
	"returns a MenuMorph from my menuSpec"
	^ self menuFrom: [ self menuSpecAt: aName ]
]

{ #category : '*Morphic-Widgets' }
PragmaMenuBuilder >> menuEntitled: aTitle [
	"returns a MenuMorph build from my menuSpec"
	^ self menuFrom: [ self menuSpec label: aTitle ]
]

{ #category : '*Morphic-Widgets' }
PragmaMenuBuilder >> menuFrom: specBlock [

	^ [ specBlock value asMenuMorph ]
			on: Error
			do: [
				self reset.
				self fallbackMenu ]
]
