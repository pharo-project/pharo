queries
usingMethods
	"All methods that read or write the slot"
	^self definingClass 
		ifNil: [ #() ]
		ifNotNil: [:class | class allMethodsAccessingSlot: self].