running
setUp
	element := 33.
	elementNotIn := 666.
	elementTwiceIn := 3.
	expectedSizeAfterReject := 1.
	expectedElementByDetect := -2.
	nonEmpty5ElementsWithoutDuplicate := Heap
		new 
		add: 2;
		add: 98;
		add: 4;
		add: 25;
		add: 1;
		yourself.
	collectionWithElement := Heap new.
	{  4. 5. 6. 2. 1. 1. (self element)  } do: [ :nb | collectionWithElement add: nb ].
	collectionWith4Elements := Heap
		new
		add: 1;
		add: -2;
		add: 3;
		add: 1;
		yourself.
	otherCollection := Heap new
		add: 1;
		add: 20;
		add: 30;
		yourself.
	empty := Heap new.
	nonEmpty := Heap
		new
		add: self valuePutIn;
		add: self element;
		add: self elementTwiceIn;
		add: self elementTwiceIn;
		yourself.
	collectionNotIncluded := Heap new
		add: elementNotIn;
		add: elementNotIn;
		yourself.
	doWithoutNumber := 3.
	collectResult := collectionWith4Elements collect: [ :each | each + 1 ].
	speciesClass := Heap.
	sameAtEndAndBegining := Heap new add: 1.5 ;  add: 1.5 copy ; yourself.
	nonEmpty1Element := Heap new add: 5 ; yourself.
	floatCollection := Heap new add: 2.5 ; add: 5.5 ; add:4.2 ; yourself.
	indexArray := #( 1 3).
	subCollection := Heap new.
	duplicateElement := 1.
	collectionWithDuplicateElement := Heap new add: duplicateElement ; add: duplicateElement ; add:4 ; yourself.
	self firstIndex to: self secondIndex do: [:each | subCollection add: elementNotIn  ].		
