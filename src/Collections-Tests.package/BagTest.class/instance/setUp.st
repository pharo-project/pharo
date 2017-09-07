running
setUp
	empty := self speciesClass new.
	nonEmpty := self speciesClass new
		add: 13;
		add: -2;
		add: self elementTwiceIn;
		add: 10;
		add: self elementTwiceIn;
		add: self element;
		yourself.
	elementNotIn := 0.
	collectionIn := self speciesClass new
		add: -2;
		add: self elementTwiceIn;
		add: 10;
		yourself.
	collectionNotIn := self speciesClass new
		add: self elementNotIn;
		add: 5;
		yourself.
	collectionOfString := self speciesClass new
		add: 1.5;
		add: 5.5;
		add: 7.5;
		yourself.
	otherCollectionWithoutEqualElements := self speciesClass new
		add: 1;
		add: 20;
		add: 30;
		add: 40;
		yourself.
	collectionWithoutNilMoreThan5 := self speciesClass new
		add: 1;
		add: 2;
		add: 3;
		add: 4;
		add: 5;
		add: 6;
		yourself.
	collectResult := self speciesClass new
		add: SmallInteger;
		add: SmallInteger;
		add: SmallInteger;
		add: SmallInteger;
		add: SmallInteger;
		add: SmallInteger;
		yourself.
	emptyButAllocatedWith20 := self speciesClass new: 20.
	collectionWithElement := self speciesClass new
		add: self element;
		yourself.
	collectionWithCharacters := self speciesClass new
		add: $p;
		add: $v;
		add: $i;
		add: $y;
		yourself