source mapping
rangeForPC: concretePC contextIsActiveContext: contextIsActive
  	"Answer the indices in the source code for the supplied pc."
	
	| pc |
	
	self sortedSourceMap 
		ifEmpty: [ ^(1 to: 0) ].

	pc := self abstractPCFor: concretePC when: contextIsActive.

	(self sortedSourceMap first key > pc)
		ifTrue: [ ^self sortedSourceMap first value ].
	
	^self abstractSourceMap 
		at: pc 
		ifAbsent: [
			(self sortedSourceMap 
				findBinary: [:assoc | pc - assoc key ] 
				ifNone: [ | end |
					end := self sortedSourceMap last value last.
					nil -> (end +1 to: end)]) value ]