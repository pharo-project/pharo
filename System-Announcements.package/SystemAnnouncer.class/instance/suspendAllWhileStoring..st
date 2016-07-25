announce
suspendAllWhileStoring: aBlock
	| reentring |
	" Suspend all the announcements, storing them in an OrderedCollection, then returns this collection"
	
	reentring := storedAnnouncements isNotNil.
	
	reentring ifFalse:[
		storedAnnouncements := OrderedCollection new.
	].

	[ 
		self suspendAllWhile: aBlock.
		^ storedAnnouncements.
	] ensure:[
		reentring ifFalse:[
			storedAnnouncements := nil.
		]
	]