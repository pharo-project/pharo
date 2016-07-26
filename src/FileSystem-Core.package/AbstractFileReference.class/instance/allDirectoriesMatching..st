enumerating
allDirectoriesMatching: aPattern
    "Return all the directories recursively nested in the receiver."
    ^ (SelectVisitor 
			breadthFirst: self resolve 
			select: [:entry | entry isDirectory and: [ aPattern match: entry basename ] ])
        collect: [ :each | each reference ]
