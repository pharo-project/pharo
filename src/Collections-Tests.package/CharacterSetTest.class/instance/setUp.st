running
setUp
	empty := self classToBeTested  new.
	full := self classToBeTested  with: $x with: $y with: $z with: $w.
	collectionIncluded := self classToBeTested  with: $y with: $x .
	collectionWith3Elements := self classToBeTested  new.
	collectionWith3Elements add: $a; add: $b; add: $c. 
	collectResult := self classToBeTested new.
	emptyButAllocatedWith20 := self classToBeTested new.
	elementNotIn := $q.
	collectionOfFloat := nil.
	nonEmpty1element := self classToBeTested  with: $ .
	withoutEqualElements := self classToBeTested  with: $r with: $s with: $w.
	collection5Elements := self classToBeTested with: $T with: $U with: $Y with: $W with: $X.