private
lookInFolder: folder

	| result |

	self flag: 'I should be #lookInFolder:forFilesMatching:, so that #lookInImageFolder can delegate to me, like the other two lookup methods'.
	result := OrderedCollection new.
	folder isDirectory ifFalse: [ ^ nil ].
	
	"To filter resources files such as .DS_Store"
	folder fileNames do: [:name |
		name first = $.
			ifFalse: [ result add: (folder / name)]].

	^ result isEmpty
		ifTrue: [ nil ]
		ifFalse: [ result ]