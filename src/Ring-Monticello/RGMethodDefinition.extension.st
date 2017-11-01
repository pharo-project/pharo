Extension { #name : #RGMethodDefinition }

{ #category : #'*ring-monticello' }
RGMethodDefinition >> sameAsMCDefinition: anMCMethodDefinition [
	^ anMCMethodDefinition selector = self selector
		and:
			[ 
			anMCMethodDefinition className = self className
				and:
					[ 
					anMCMethodDefinition classIsMeta = self isMetaSide
						and:
							[ 
							anMCMethodDefinition category = self protocol
								and: [ anMCMethodDefinition source = self sourceCode ] ] ] ]
]
