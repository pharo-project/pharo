private
scanForKeyOrNil: anObject
	"Same as scanFor: , but treats association with key == nil as empty slot"
	
	| element start finish |
	finish := array size.
	start := self startIndexFor: anObject.
	
	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element := array at: index) == nil or: [ element key isNil or: [self compare: element key to: anObject]])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element := array at: index) == nil or: [ element key isNil or: [self compare: element key to: anObject]])
			ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"