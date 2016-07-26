enumerating
dates


	| dates |

	dates := OrderedCollection new.
	self datesDo: [ :m | dates add: m ].
	^ dates asArray.