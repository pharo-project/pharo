deprecated
updatePackageMethodIn: aConfig sourceVersion: sourceVersionString

	self deprecated: 'see MetacelloToolBox class>>updateDevelopment:for:updateProjects:description: for replacement method'.
	self 
		prepareForMethodUpdate: aConfig 
		sourceVersion: sourceVersionString 
		forceUpdate: false
		generating: [:selector :pragmaMap :ar :updatedPackageSpecsMap | | strm pragma |
			updatedPackageSpecsMap isEmpty ifTrue: [ ^false ].
			strm := WriteStream on: String new.
			strm 
				nextPutAll: selector asString, ' spec'; cr;
				tab; nextPutAll: '<version: ', sourceVersionString printString.
			pragma := pragmaMap at: selector.
			pragma numArgs = 2
				ifTrue: [
					strm nextPutAll: ' imports: #('.
					(pragma argumentAt: 2) do: [:versionString |
						strm nextPutAll: versionString printString; space ]].
			strm nextPutAll: ')>';cr.
			(ar at: 2) do: [:attribute | | vs d |
				vs := (ar at: 1) at: attribute.
				(d := updatedPackageSpecsMap at: attribute ifAbsent: []) ~~ nil
					ifTrue: [ 
						vs packagesSpec list do: [:member | 
							member spec file ~~ nil
								ifTrue: [ | x |
									(((x := d at: member spec name ifAbsent: []) ~~ nil) and: [ x ~~ #uptodate ])
						 				ifTrue: [ member spec file: x file ]]]].
				strm cr; tab; nextPutAll: 'spec for: ', attribute printString, ' do: ['; cr.
				attribute == #common
					ifTrue: [ 
						vs 
							author: MetacelloPlatform current authorName;
							timestamp: MetacelloPlatform current timestamp ].
				vs configMethodOn: strm indent: 2.
				strm nextPutAll: '].'].
			(aConfig class	
					compile: strm contents
					classified: (aConfig class whichCategoryIncludesSelector: pragma selector)) == nil
						ifTrue: [ self error: 'Error compiling the method' ]].
	^true