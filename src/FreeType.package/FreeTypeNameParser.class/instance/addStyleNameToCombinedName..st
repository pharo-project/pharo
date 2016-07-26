parsing
addStyleNameToCombinedName: aStyleString
	| lcCombined lcStyleName addStyle index |
	lcCombined := combinedName asLowercase.
	lcStyleName := aStyleString asLowercase.
	addStyle := true.
	(index := lcCombined findString: lcStyleName) > 0
		ifTrue:[ 
			(index = 1 or:[delimiters includes: (lcCombined at: index - 1)])
				ifTrue:[
					((index + lcStyleName size > lcCombined size) or:[ delimiters includes: (lcCombined at: index + lcStyleName size) ])
						ifTrue:["don't add the style to the combinedName, because it already contains it"
							addStyle := false]]].
	addStyle ifTrue:[combinedName := combinedName , ' ', aStyleString].
	