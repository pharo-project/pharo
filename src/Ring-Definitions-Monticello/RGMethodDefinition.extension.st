Extension { #name : 'RGMethodDefinition' }

{ #category : '*Ring-Definitions-Monticello' }
RGMethodDefinition >> sameAsMCDefinition: anMCMethodDefinition [

	^ anMCMethodDefinition selector = self selector and: [
		  anMCMethodDefinition className = self className and: [
			  anMCMethodDefinition classIsMeta = self isMeta and: [ anMCMethodDefinition protocol = self protocol and: [ anMCMethodDefinition source = self sourceCode ] ] ] ]
]
