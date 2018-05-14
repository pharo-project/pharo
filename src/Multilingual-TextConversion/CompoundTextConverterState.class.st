"
This represents the state of CompoundTextConverter.
"
Class {
	#name : #CompoundTextConverterState,
	#superclass : #Object,
	#instVars : [
		'g0Size',
		'g1Size',
		'g0Leading',
		'g1Leading',
		'charSize',
		'streamPosition'
	],
	#category : #'Multilingual-TextConversion-Utilities'
}

{ #category : #'instance creation' }
CompoundTextConverterState class >> g0Size: g0 g1Size: g1 g0Leading: g0l g1Leading: g1l charSize: cSize streamPosition: pos [

	^ (self new)
		g0Size: g0
		g1Size: g1
		g0Leading: g0l
		g1Leading: g1l
		charSize: cSize
		streamPosition: pos
	; yourself.

]

{ #category : #accessing }
CompoundTextConverterState >> charSize [

	^ charSize

]

{ #category : #accessing }
CompoundTextConverterState >> charSize: s [

	charSize := s.

]

{ #category : #accessing }
CompoundTextConverterState >> g0Leading [

	^ g0Leading

]

{ #category : #accessing }
CompoundTextConverterState >> g0Leading: l [

	g0Leading := l.

]

{ #category : #accessing }
CompoundTextConverterState >> g0Size [

	^ g0Size

]

{ #category : #accessing }
CompoundTextConverterState >> g0Size: s [

	g0Size := s.

]

{ #category : #accessing }
CompoundTextConverterState >> g0Size: g0 g1Size: g1 g0Leading: g0l g1Leading: g1l charSize: cSize streamPosition: pos [

	g0Size := g0.
	g1Size := g1.
	g0Leading := g0l.
	g1Leading := g1l.
	charSize := cSize.
	streamPosition := pos.

]

{ #category : #accessing }
CompoundTextConverterState >> g1Leading [

	^ g1Leading

]

{ #category : #accessing }
CompoundTextConverterState >> g1Leading: l [

	g1Leading := l.

]

{ #category : #accessing }
CompoundTextConverterState >> g1Size [

	^ g1Size

]

{ #category : #accessing }
CompoundTextConverterState >> g1Size: s [

	g1Size := s.

]

{ #category : #printing }
CompoundTextConverterState >> printOn: aStream [

	aStream nextPut: $(;
		nextPutAll: g0Size printString; space;
		nextPutAll: g1Size printString; space;
		nextPutAll: g0Leading printString; space;
		nextPutAll: g1Leading printString; space;
		nextPutAll: charSize printString; space;
		nextPutAll: streamPosition printString.
	aStream nextPut: $).

]

{ #category : #accessing }
CompoundTextConverterState >> streamPosition [

	^ streamPosition

]

{ #category : #accessing }
CompoundTextConverterState >> streamPosition: pos [

	streamPosition := pos.

]
