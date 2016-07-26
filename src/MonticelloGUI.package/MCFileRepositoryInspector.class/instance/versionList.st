morphic ui
versionList
	| result sortBlock |
	result := selectedPackage 
		ifNil: [ versions ifNil: [ ^ #() ]]
		ifNotNil: [ (versions ifNil: [ ^ #() ]) select: [ :each | selectedPackage = each first ] ].
	sortBlock := (self orderSpecs at: order) value.
	sortBlock ifNotNil: [
		result := result asSortedCollection: [:a :b | [sortBlock value: a value: b] on: Error do: [true]]].
	
	versionPattern ifNotEmpty: [
		result := result select: [ :package| package fourth asLowercase includesSubstring: versionPattern ]].
					
	^ result collect: [ :each | self versionHighlight: each fourth ]