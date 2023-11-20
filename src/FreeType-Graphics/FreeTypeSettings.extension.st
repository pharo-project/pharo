Extension { #name : 'FreeTypeSettings' }

{ #category : '*FreeType-Graphics' }
FreeTypeSettings >> bitBltSubPixelAvailable [
	"Answer true if the the subPixel combination rule is available, false otherwise.
	to test :-
	bitBltSubPixelAvailable := false.
	FreeTypeCache current removeAll.
	World restoreMorphicDisplay
	"

	| form bitBlt color |
	bitBltSubPixelAvailable ifNotNil: [ ^ bitBltSubPixelAvailable ].
	form := Form extent: 10 @ 10 depth: 32.
	bitBlt := GrafPort toForm: form.
	bitBlt combinationRule: 41.
	bitBlt sourceForm: (Form extent: 5 @ 5 depth: 32).
	bitBlt destOrigin: 1 @ 1.
	bitBlt
		width: 5;
		height: 5.
	color := Color black asNontranslucentColor pixelValueForDepth: 32.
	[
	bitBlt
		copyBitsColor: (color bitAnd: 16rFFFFFF)
		alpha: (color bitAnd: 16rFF000000) >> 24
		gammaTable: nil
		ungammaTable: nil ]
		on: Error
		do: [ :e | ^ bitBltSubPixelAvailable := false ].
	#toDo. "need to check that rule 41 has done the right thing, and isn't someone elses new BitBlt rule"
	^ bitBltSubPixelAvailable := true
]

{ #category : '*FreeType-Graphics' }
FreeTypeSettings >> glyphContrast: aContrastValue [
	" value between 1 and 100.
	100 is highest contrast and maps to gamma 0.25
	1 is lowest contrast and maps to gamma 2.22"
	Cursor wait showWhile: [
		| v g |
		v := (((aContrastValue asNumber) min: 100) max: 1) asFloat.
		(v closeTo: 50.0)
			ifTrue: [g := 1.0]
			ifFalse: [g := ((100 - v) + 50 / 100.0) raisedTo: 2].
		self setGamma: g]
]
