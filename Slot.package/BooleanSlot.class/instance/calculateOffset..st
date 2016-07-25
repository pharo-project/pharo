class building
calculateOffset: aClass
	| booleanSlots  calculatedOffset |
	"first we get all boolean slots and sort them by index"
	booleanSlots := aClass allSlots select: [ :each | each isKindOf: self class ].
	booleanSlots := booleanSlots select: [ :each | each offset isNotNil ].
	booleanSlots sort: [ :a :b | a offset < b offset ]. 
	booleanSlots ifEmpty: [ ^1 ]. "we are the first one!"

	"now we find the first spot where the next offset is not yet taken"
	calculatedOffset := (booleanSlots withIndexCollect: [ :each :myIndex | myIndex ->each offset ]) 
		findFirst: [ :each | each key ~= each value ].
	calculatedOffset = 0  "no hole found: use the next index after the last slot"
		ifTrue: [ ^ booleanSlots last offset + 1 ].
	^calculatedOffset
	