results
preamble
	| preamble |
	"Very simplistic now: needs to do some optimizatons"
	
	preamble := OrderedCollection new.
	links do: [:link |  
		plugins do: [ :plugin | (link allReifications includes: plugin key) ifTrue: [preamble addAll: ((plugin entity: entity link: link) preamble: entity)]].
		link control = #instead ifTrue: [
				"for instead links, the preamble needs to clean the stack. For now just implemented for message sends"
				entity isMessage ifTrue: [
					entity numArgs + 1 timesRepeat: [preamble add: (RFStorePopIntoTempNode named: #RFBalancestack)]]]].
	^preamble