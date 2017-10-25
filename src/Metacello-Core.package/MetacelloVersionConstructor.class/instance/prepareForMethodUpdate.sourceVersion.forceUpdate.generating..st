deprecated
prepareForMethodUpdate: aConfig sourceVersion: sourceVersionString forceUpdate: forceUpdate generating: generateBlock
	| pragmaDict versionSpecs pragmaMap updatedPackageSpecs updatedPackageSpecsMap reversed pragmaColl seenUpdatedPackageSpecs |
	self deprecated: 'see MetacelloToolBox for replacement methods'.
	self configuration: aConfig.
	versionSpecs := Dictionary new.
	pragmaMap := Dictionary new.
	pragmaDict := self extractAllVersionPragmas.
	pragmaColl := pragmaDict at: sourceVersionString ifAbsent: [ ^ self ].
	pragmaColl
		do: [ :pragma | 
			| specs versionSpec |
			specs := Dictionary new.
			self evaluatePragma: pragma.
			self attributeMap
				keysAndValuesDo: [ :attribute :blockList | 
					versionSpec := self project versionSpec.
					versionSpec versionString: sourceVersionString.
					specs at: attribute put: versionSpec.
					blockList do: [ :block | self with: versionSpec during: block ] ].
			versionSpecs
				at: pragma selector
				put:
					{specs.
					(self attributeOrder).
					nil}.
			pragmaMap at: pragma selector put: pragma.
			self reset ].
	versionSpecs
		keysAndValuesDo: [ :selector :ar | 
			updatedPackageSpecsMap := Dictionary new.
			seenUpdatedPackageSpecs := Dictionary new.
			ar at: 3 put: updatedPackageSpecsMap.
			reversed := aConfig project attributes reverse.
			reversed
				do: [ :attribute | 
					| vs |
					(vs := (ar at: 1) at: attribute ifAbsent: [  ]) ~~ nil
						ifTrue: [ 
							updatedPackageSpecs := forceUpdate
								ifTrue: [ vs forceUpdatedPackageSpecs ]
								ifFalse: [ vs updatedPackageSpecs ].
							updatedPackageSpecs associations
								do: [ :assoc | 
									| filename |
									(filename := seenUpdatedPackageSpecs at: assoc key ifAbsent: [  ]) == nil
										ifTrue: [ 
											assoc value == #uptodate
												ifTrue: [ 
													"#uptodate means that the spec is up-to-date and we mark it as
										 seen so that the spec is not update for a 'later' spec"
													seenUpdatedPackageSpecs at: assoc key put: #uptodate ]
												ifFalse: [ seenUpdatedPackageSpecs at: assoc key put: assoc value file ] ]
										ifFalse: [ 
											"if the spec was already seen as up-to-date or the file is the same as 
								 the one already seen don't propogate the file"
											(filename == #uptodate or: [ assoc value == #uptodate or: [ filename = assoc value file ] ])
												ifTrue: [ updatedPackageSpecs removeKey: assoc key ] ] ].
							updatedPackageSpecsMap at: attribute put: updatedPackageSpecs ] ].	"clear out #uptodate markers"
			updatedPackageSpecsMap
				valuesDo: [ :d | 
					d associations
						do: [ :assoc | 
							assoc value == #uptodate
								ifTrue: [ d removeKey: assoc key ] ] ].	"remove shadowed packages"
			1 to: reversed size do: [ :index | 
				| attribute d |
				attribute := reversed at: index.
				((d := updatedPackageSpecsMap at: attribute ifAbsent: [  ]) ~~ nil and: [ d keys size > 0 ])
					ifTrue: [ 
						index + 1 to: reversed size do: [ :shadowIndex | 
							d
								keysDo: [ :key | 
									| dict |
									(dict := updatedPackageSpecsMap at: (reversed at: shadowIndex) ifAbsent: [  ]) ~~ nil
										ifTrue: [ dict removeKey: key ifAbsent: [  ] ] ] ] ] ].
			updatedPackageSpecsMap keys
				do: [ :key | 
					| d |
					d := updatedPackageSpecsMap at: key.
					d isEmpty
						ifTrue: [ updatedPackageSpecsMap removeKey: key ] ].	"ready to generate source for method"
			generateBlock
				value: selector
				value: pragmaMap
				value: ar
				value: updatedPackageSpecsMap ]