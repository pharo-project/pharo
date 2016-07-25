services
newChangesFromStream: aStream named: aName
	"File in the code from the stream into a new change set whose
	name is derived from aName. Leave the 'current change set'
	unchanged. Return the new change set or nil on failure."

	| oldChanges newName newSet |
	oldChanges := self current.
	newName := aName withoutPeriodSuffix.
	newSet := self basicNewChangeSet: newName.
	[ | newStream |
	      newSet ifNotNil:[
			(aStream respondsTo: #converter:) ifFalse: [
				newStream := MultiByteBinaryOrTextStream with: (aStream contentsOfEntireFile).
				newStream reset.] ifTrue: [newStream := aStream].
			self newChanges: newSet.
			newStream setConverterForCode.
			CodeImporter evaluateReadStream: newStream readStream].
	aStream close] ensure: [self newChanges: oldChanges].
	^ newSet