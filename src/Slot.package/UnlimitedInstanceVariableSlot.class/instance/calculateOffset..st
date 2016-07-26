class building
calculateOffset: aClass
	| ivarSlots  calculatedOffset |
	"first we get all boolean slots and sort them by index"
	ivarSlots := aClass allSlots select: [ :each | each isKindOf: self class ].
	ivarSlots := ivarSlots select: [ :each | each offset isNotNil ].
	ivarSlots sort: [ :a :b | a offset < b offset ]. 
	ivarSlots ifEmpty: [ ^1 ]. "we are the first one!"

	"now we find the first spot where the next offset is not yet taken"
	calculatedOffset := (ivarSlots withIndexCollect: [ :each :myIndex | myIndex ->each offset ]) 
		findFirst: [ :each | each key ~= each value ].
	calculatedOffset = 0  "no hole found: use the next index after the last slot"
		ifTrue: [ ^ ivarSlots last offset + 1].
	^calculatedOffset
	