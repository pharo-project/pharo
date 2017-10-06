running
setUp
	emptyDict := self classToBeTested new.
	nonEmptyDict := self classToBeTested new.
	nonEmptyDict
		at: #a
			put: self elementTwiceIn;
		at: #b
			put: 30;
		at: #c
			put: self elementTwiceIn;
		at: #d
			put: -2.
	nonEmpty5ElementsNoDuplicates := self classToBeTested new
		at: #a
			put: 5;
		at: #b
			put: 4;
		at: #c
			put: 7;
		at: #d
			put: 6;
		at: #e
			put: 9;
		yourself.
	valueNotIn := 666.
	keyNotIn := #z .
	associationNotIn := keyNotIn->valueNotIn.
	dictionaryNotIncluded := Dictionary new add: associationNotIn ;yourself.
	collectionNotIncluded := {  valueNotIn. valueNotIn  }.
	collectionIncluded := {  (self elementTwiceIn)  }.
	indexArray := #(2 3 1 ).
	valueArray := #(5 5 5 ).
	nonEmpty1Element := self classToBeTested new
		at: #a
			put: 5;
		yourself.
	nonEmptyWithFloat := Dictionary new add: #A->2.5; add: #b->3.5 ; yourself.
	duplicateValue := 2.5.
	dictionaryWithDuplicateValues := 	Dictionary new add: #A->duplicateValue ; add: #b->3.5 ; add: #C->duplicateValue  ; yourself.

