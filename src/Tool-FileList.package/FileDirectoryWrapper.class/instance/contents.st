accessing
contents

	^((model directoriesFor: item) 
		sort: [ :a :b | a basename caseInsensitiveLessOrEqual: b basename]) 
		collect: [ :directory| 
			self class with: directory name: directory basename model: self ]
