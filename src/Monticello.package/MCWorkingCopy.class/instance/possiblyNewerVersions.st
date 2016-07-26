private
possiblyNewerVersions

	^Array streamContents: [:strm |
		self repositoryGroup repositories do: [:repo |
			strm nextPutAll: (self possiblyNewerVersionsIn: repo)]]