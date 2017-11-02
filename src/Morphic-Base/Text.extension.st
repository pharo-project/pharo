Extension { #name : #Text }

{ #category : #'*Morphic-Base-Converting' }
Text >> asMorph [
	^ self asTextMorph
]

{ #category : #'*Morphic-Base-Converting' }
Text >> asStringMorph [
	^ StringMorph
		contents: self string
		font: (self fontAt: 1 withStyle: TextStyle default)
		emphasis: (self emphasisAt: 1)
]

{ #category : #'*Morphic-Base-Converting' }
Text >> asTextMorph [
	^ TextMorph new contentsAsIs: self
]

{ #category : #'*Morphic-Base-Pluggable Widgets' }
Text >> beginsWith: aString fromList: aMorph [

	^ self asString beginsWith: aString fromList: aMorph
]

{ #category : #'*Morphic-Base' }
Text >> embeddedMorphs [
	"return the list of morphs embedded in me"

	| morphs |
	morphs := IdentitySet new.
	runs withStartStopAndValueDo: 
			[:start :stop :attribs | 
			attribs 
				do: [:attrib | attrib anchoredMorph ifNotNil: [morphs add: attrib anchoredMorph]]].
	^morphs select: [:m | m isMorph]
]

{ #category : #'*Morphic-Base' }
Text >> embeddedMorphsFrom: start to: stop [ 
	"return the list of morphs embedded in me"

	| morphs |
	morphs := IdentitySet new.
	runs 
		runsFrom: start
		to: stop
		do: 
			[:attribs | 
			attribs 
				do: [:attr | attr anchoredMorph ifNotNil: [morphs add: attr anchoredMorph]]].
	^morphs select: [:m | m isMorph]
]

{ #category : #'*Morphic-Base-Widgets' }
Text >> heightToDisplayInList: aList [

	^ aList font height
]

{ #category : #'*Morphic-Base-Widgets' }
Text >> listRenderOn: aCanvas atRow: aRow bounds: bounds color: color backgroundColor: backgroundColor from: aMorph [
	"Method extracted from LazyListMorph"
	| attrs useDefaultFont f itemBounds |
	
	attrs := self attributesAt: 1.
	useDefaultFont := true.
	attrs
		do: [:att | att
				forFontInStyle: aMorph font textStyle
				do: [:fon | useDefaultFont := false]].
	f := useDefaultFont
				ifTrue: [ aMorph font]
				ifFalse: [ self fontAt: 1 withStyle: aMorph font textStyle ].
	itemBounds := bounds withHeight: f height.
	itemBounds := itemBounds align: itemBounds leftCenter with: bounds leftCenter.
	"center the item if the font height is different to that of our font"
	aCanvas
		drawString: self
		in: itemBounds
		font: (f emphasized: (self emphasisAt: 1))
		color: color
		underline: ((self emphasisAt: 1)
				bitAnd: 4)
				> 0
		underlineColor: color
		strikethrough: ((self emphasisAt: 1)
				bitAnd: 16)
				> 0
		strikethroughColor: color
]

{ #category : #'*Morphic-Base-Widgets' }
Text >> widthToDisplayInList: aList [

	^ aList font widthOfStringOrText: self contents
]
