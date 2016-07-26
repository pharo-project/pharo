repositories
checkRepositoryTemplates
	"unused for now - we only do HTTP"
	| bad |
	bad := self repositories select: [:repo | repo creationTemplate isNil].
	^bad isEmpty or: [
		self selectRepository: bad first.
		self inform: (String streamContents: [:strm |
			strm nextPutAll: 'Creation template missing for'; cr.
			bad do: [:r | strm nextPutAll: r description; cr].
			strm nextPutAll: 'Please fill in the details first!']).
		false].
