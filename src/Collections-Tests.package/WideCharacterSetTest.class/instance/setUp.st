running
setUp
	empty := self classToBeTested  new.
	full := self classToBeTested  with: (Character codePoint: 257) with: (Character codePoint: 258) with: (Character codePoint: 259) with: (Character codePoint: 260).
	collectionIncluded := self classToBeTested  with: (Character codePoint: 257) with: (Character codePoint: 258).
	collectionWith3Elements := self classToBeTested  new.
	collectionWith3Elements add: (Character codePoint: 267); add: (Character codePoint: 268). 
	collectResult := self classToBeTested new.
	emptyButAllocatedWith20 := self classToBeTested new.
	elementNotIn := Character codePoint: 475.
	collectionOfFloat := nil.
	nonEmpty1element := self classToBeTested  with: (Character codePoint: 301).
	withoutEqualElements := self classToBeTested  
		withAll: ((Character codePoint: 277) to: (Character codePoint: 279)).
	collection5Elements := self classToBeTested 
		withAll: ((Character codePoint: 295) to: (Character codePoint: 299))