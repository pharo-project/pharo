Extension { #name : #PragmaMenuBuilder }

{ #category : #'*Morphic-Base' }
PragmaMenuBuilder >> fallbackMenu [

	^ FallbackMenu when: self fails: model.
]

{ #category : #'*Morphic-Base' }
PragmaMenuBuilder >> menu [
	"returns a MenuMorph build from my menuSpec"
	
	^ self menuFrom: [ self menuSpec ].
]

{ #category : #'*Morphic-Base' }
PragmaMenuBuilder >> menuAt: aName [
	"returns a MenuMorph from my menuSpec"
	^ self menuFrom: [ self menuSpecAt: aName ].
]

{ #category : #'*Morphic-Base' }
PragmaMenuBuilder >> menuEntitled: aTitle [ 
	"returns a MenuMorph build from my menuSpec"
	^ self menuFrom: [ self menuSpec label: aTitle ].
]

{ #category : #'*Morphic-Base' }
PragmaMenuBuilder >> menuFrom: specBlock [

	^ [ specBlock value asMenuMorph ]
			on: Error
			do: [
				self reset.
				self fallbackMenu ].
]
