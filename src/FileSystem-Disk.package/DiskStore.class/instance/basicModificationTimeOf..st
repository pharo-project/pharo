private
basicModificationTimeOf: anEntry
	" the entry contains the seconds since the squeak epoch in local time"
	^ (DateAndTime fromSeconds: (anEntry at: 3) offset: 0) translateTo: DateAndTime localOffset