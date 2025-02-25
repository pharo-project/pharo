Extension { #name : 'RGGlobalVariable' }

{ #category : '*PharoBootstrap' }
RGGlobalVariable >> isInstanceVariable [
	"This method is missing in Pharo < 13 so we add it as an extension. When the bootstrap will happen in Pharo 13+ we can remove this method"
	^ false
]
