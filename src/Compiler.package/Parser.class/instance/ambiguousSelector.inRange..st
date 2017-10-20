error correction
ambiguousSelector: aString inRange: anInterval
	| correctedSelector userSelection offset intervalWithOffset |
	
	self interactive ifFalse: [
		"In non interactive mode, break backward compatibility: $- is part of selector"
		Transcript cr; nextPutAll: encoder classEncoding storeString; nextPutAll:#'>>'; nextPutAll: encoder selector storeString; show: ' will send ' , token , '-'.
		^super ambiguousSelector: aString inRange: anInterval].
	
	"handle the text selection"
	userSelection := requestor selectionInterval.
	intervalWithOffset := anInterval first + requestorOffset to: anInterval last + requestorOffset.
	requestor selectFrom: intervalWithOffset first to: intervalWithOffset last.
	requestor select.

	"Build the menu with alternatives"
	correctedSelector := AmbiguousSelectorDetected 
			signalName: aString
			inRange: intervalWithOffset.
	correctedSelector ifNil: [^self fail].

	"Execute the selected action"
	offset := self substituteWord: correctedSelector wordInterval: intervalWithOffset offset: 0.
	requestor deselect.
	requestor selectInvisiblyFrom: userSelection first to: userSelection last + offset.
	token := (correctedSelector readStream upTo: Character space) asSymbol