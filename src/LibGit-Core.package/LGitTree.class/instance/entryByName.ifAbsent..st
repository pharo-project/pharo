accessing
entryByName: aString ifAbsent: aBlock
	| entry |
	entry := self tree_entry_byname: self name: aString.
	(entry isNil or: [ entry isNull ])
		ifTrue: [ ^aBlock value ].
	entry repository: self repository.
	^entry