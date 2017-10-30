*Text-Edition
addNewFontSize: pointSize
	"Add a font in specified size to the array of fonts."
	| f d newArray t  |
	fontArray first emphasis ~= 0 ifTrue: [
		t := TextSharedInformation at: self fontArray first familyName asSymbol.
		t fonts first emphasis = 0 ifTrue: [
			^ t addNewFontSize: pointSize.
		].
	].

	pointSize <= 0 ifTrue: [^ nil].
	fontArray do: [:s |
		s pointSize = pointSize ifTrue: [^ s].
	].
	f := fontArray first class new initialize: fontArray first.
	f pointSize: pointSize.
	fontArray first derivativeFonts do: [:proto |
			proto ifNotNil: [
				d := proto class new initialize: proto.
				d pointSize: f pointSize.
				f derivativeFont: d mainFont: proto.
			].
	].
	newArray := ((fontArray copyWith: f) asSortedCollection: [:a :b | a pointSize <= b pointSize]) asArray.
	self newFontArray: newArray.
	^ self fontOfPointSize: pointSize
