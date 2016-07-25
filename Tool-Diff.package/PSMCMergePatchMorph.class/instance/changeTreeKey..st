as yet unclassified
changeTreeKey: aCharacter
	"Handle change tree key strokes."

	super changeTreeKey: aCharacter.
	(aCharacter == $a and: [ self selectionIsNotNil]) ifTrue: [
		^ self toggleApplySelectedChange.
		].
	(aCharacter == $c and: [self selectionIsNotNil]) ifTrue: [
		^self keepCurrentVersion].
	(aCharacter == $g and: [self selectionIsNotNil]) ifTrue: [
		^self useIncomingVersion].
	(aCharacter == $x and: [self selectionIsNotNil]) ifTrue: [
		^self markAsConflict].
	(aCharacter == $f and: [self notAllConflictsResolved]) ifTrue: [
		^self selectNextConflict].
	
	(aCharacter == $C and: [self hasAnyNonKeptConflicts]) ifTrue: [
		^self keepAllCurrentVersion].
	(aCharacter == $G and: [self hasAnyNonIncomingConflicts]) ifTrue: [
		^self useAllIncomingVersion].
	(aCharacter == $X and: [self hasAnyNonConflictConflicts]) ifTrue: [
		^self markAllAsConflict]