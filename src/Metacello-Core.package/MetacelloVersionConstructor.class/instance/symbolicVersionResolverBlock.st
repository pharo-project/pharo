private
symbolicVersionResolverBlock
	^ [ :symbolicVrsn :pragma | 
	| result |
	result := nil.
	(pragma argumentAt: 1) = symbolicVrsn
		ifTrue: [ 
			self symbolicVersion: symbolicVrsn.
			self evaluatePragma: pragma.
			self project attributes
				do: [ :attribute | 
					| versionString |
					versionString := self attributeMap at: attribute ifAbsent: [  ].
					versionString ~~ nil
						ifTrue: [ result := versionString ] ] ].
	result ]