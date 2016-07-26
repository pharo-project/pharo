menu
setDefaultFonts: defaultFontsSpec
	"Since this is called from menus, we can take the opportunity to prompt for missing font styles."

	| fontNames map emphases |
	fontNames := defaultFontsSpec collect: [:array | array second].
	map := IdentityDictionary new.
	emphases := IdentityDictionary new.
	fontNames do: [:originalName | | decoded style |
		decoded := TextStyle decodeStyleName: originalName.
		style := map at: originalName put: (TextStyle named: decoded second).
		emphases at: originalName put: decoded first.
		style ifNil: [map at: originalName put: (TextStyle default) ]].
	defaultFontsSpec do: [:triplet | self
		perform: triplet first
		with: (((map at: triplet second) fontOfPointSize: triplet third) emphasized: (emphases at: triplet second))]