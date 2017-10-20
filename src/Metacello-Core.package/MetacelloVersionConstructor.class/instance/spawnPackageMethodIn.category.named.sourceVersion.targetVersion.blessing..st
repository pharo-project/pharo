deprecated
spawnPackageMethodIn: aConfig category: methodCategory named: newSelector sourceVersion: sourceVersionString targetVersion: targetVersionString blessing: blessing

	self deprecated: 'see MetacelloToolBox class>>createBaseline:for:from:description: for replacement method'.
	self 
		prepareForMethodUpdate: aConfig 
		sourceVersion: sourceVersionString 
		forceUpdate: blessing ~~ #baseline
		generating: [:selector :pragmaMap :ar :updatedPackageSpecsMap | | strm |
			strm := WriteStream on: String new.
			strm 
				nextPutAll: newSelector asString, ' spec'; cr;
				tab; nextPutAll: 
						'<version: ', 
						targetVersionString printString, 
						' imports: #(', sourceVersionString printString, 
						')>';cr.
			(ar at: 2) do: [:attribute | | vs d |
				vs := (ar at: 1) at: attribute.
				(d := updatedPackageSpecsMap at: attribute ifAbsent: []) ~~ nil
					ifTrue: [ 
						vs packagesSpec list do: [:member | | x |
							x := d at: member spec name ifAbsent: [].
						 	member spec updateForSpawnMethod: x ]]
					ifFalse: [
						vs packagesSpec list do: [:member |
							member spec updateForSpawnMethod: member spec copy ]].
				strm cr; tab; nextPutAll: 'spec for: ', attribute printString, ' do: ['; cr.
				attribute == #common
					ifTrue: [
						vs 
							author: MetacelloPlatform current authorName;
							timestamp: MetacelloPlatform current timestamp.
						blessing ~~ nil
							ifTrue: [ vs blessing: blessing ]].
				vs configSpawnMethodOn: strm indent: 2.
				strm nextPutAll: '].'].
				(aConfig class	
					compile: strm contents
					classified: methodCategory) == nil 
						ifTrue: [ self error: 'Error compiling the method' ]].
	^true