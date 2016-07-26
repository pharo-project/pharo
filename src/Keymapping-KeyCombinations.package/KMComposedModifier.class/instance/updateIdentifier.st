accessing
updateIdentifier
	identifier := String streamContents: [ :aStream|
			modifiers do: [ :modifier| 
				aStream << modifier identifier]].
	identifier := identifier asSymbol