running
setUp
	empty := self classToBeTested  new.
	full := self classToBeTested  with: 1 with: 2 with: 3 with: 4.
	collectionIncluded := self classToBeTested  with: 2 with: 3 .
	collectionWith3Elements := self classToBeTested  new.
	collectionWith3Elements add: 1; add: -2; add: 3. 
	collectResult := self classToBeTested  new add: SmallInteger ; yourself.
	emptyButAllocatedWith20 := self classToBeTested  new: 20.
	elementNotIn := 99.
	collectionOfFloat := self classToBeTested  with: 2.5 with: 4.6 with: 4.2.
	nonEmpty1element := self classToBeTested  with: 32.
	withoutEqualElements := self classToBeTested  with: 4 with: 5 with: 2.
	collection5Elements := self classToBeTested with: 1 with: 2 with: 3 with: 4 with: 5.