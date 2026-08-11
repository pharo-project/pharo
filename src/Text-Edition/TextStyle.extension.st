Extension { #name : 'TextStyle' }

{ #category : '*Text-Edition' }
TextStyle >> addNewFontSize: pointSize [
	"Add a font in specified size to the array of fonts."
	| f d newArray t  |
	self fontArray first emphasis ~= 0 ifTrue: [
		t := TextSharedInformation at: self fontArray first familyName asSymbol.
		t fonts first emphasis = 0 ifTrue: [
			^ t addNewFontSize: pointSize.
		].
	].

	pointSize <= 0 ifTrue: [^ nil].
	self fontArray do: [:s |
		s pointSize = pointSize ifTrue: [^ s].
	].
	f := self fontArray first class new initialize: self fontArray first.
	f pointSize: pointSize.
	self fontArray first derivativeFonts do: [:proto |
			proto ifNotNil: [
				d := proto class new initialize: proto.
				d pointSize: f pointSize.
				f derivativeFont: d mainFont: proto.
			].
	].
	newArray := ((self fontArray copyWith: f) asSortedCollection: [:a :b | a pointSize <= b pointSize]) asArray.
	self newFontArray: newArray.
	^ self fontOfPointSize: pointSize
]

{ #category : '*Text-Edition' }
TextStyle >> isTTCStyle [

	^ self fontArray first isTTCFont
]
