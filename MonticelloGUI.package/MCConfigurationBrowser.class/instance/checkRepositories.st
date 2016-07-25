repositories
checkRepositories
	| bad |
	bad := self repositories reject: [:repo | repo isKindOf: MCHttpRepository].
	^bad isEmpty or: [
		self selectRepository: bad first.
		self inform: (String streamContents: [:strm |
			strm nextPutAll: 'Please remove these repositories:'; cr.
			bad do: [:r | strm nextPutAll: r description; cr].
			strm nextPutAll: '(only HTTP repositories are supported)']).
		false].
