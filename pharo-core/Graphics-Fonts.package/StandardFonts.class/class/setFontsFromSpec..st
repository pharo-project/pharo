font accessing
setFontsFromSpec: fontsSpec
	"set some fonts by interpreting fontsSpec.
	fontsSpec is a collection.
	Each fontsSpec element is an array with 3 elements: 
	(1) the font accessor (i.e. #codeFont:)
	(2) the font family (i.e. 'Arial')
	(3) the font point size.
	To see an example of fontsSpec, inspect the result of 
	StandardFonts getCurrentFontsSpec.
	"
	| fontNames map emphases |
	fontNames := fontsSpec
				collect: [:array | array second].
	map := IdentityDictionary new.
	emphases := IdentityDictionary new.
	fontNames
		do: [:originalName | 
			| decoded style |
			decoded := TextStyle decodeStyleName: originalName.
			style := map
						at: originalName
						put: (TextStyle named: decoded second).
			emphases at: originalName put: decoded first.
			style
				ifNil: [map at: originalName put: TextStyle default]].
	fontsSpec
		do: [:triplet | self
				perform: triplet first
				with: (((map at: triplet second)
						fontOfPointSize: triplet third)
						emphasized: (emphases at: triplet second))]