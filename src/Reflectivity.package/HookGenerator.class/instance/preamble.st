results
preamble
	| preamble |
	
	self flag: #TBD.
	"Very simplistic now: needs to do some optimizatons"
	
	preamble := OrderedCollection new.
	links do: [:link |  
		plugins do: [ :plugin | (link allReifications includes: plugin key) ifTrue: [preamble addAll: ((plugin entity: entity link: link) preamble: entity)] ]].
	^preamble