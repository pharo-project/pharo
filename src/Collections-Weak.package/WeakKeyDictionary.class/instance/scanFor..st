private
scanFor: anObject
	"mvl 10/21/2007: The method has been copied to this location to indicate that whenever #scanFor: changes #scanForEmpty: must be changed in the receiver as well."
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. Expired associations are ignored, since callers take non-nil slots as a successful search. Subclasses should override startIndexFor: and compare:to: if they have a different semantics for matching elements."
	| element start finish |
	finish := array size.
	start := self startIndexFor: anObject.
	
	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element := array at: index) == nil or: [self compare: element key to: anObject])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element := array at: index) == nil or: [self compare: element key to: anObject])
			ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"