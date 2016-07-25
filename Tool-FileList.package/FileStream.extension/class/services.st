*Tool-FileList
services

	^ Array 
			with: self serviceRemoveLineFeeds
			with: self serviceFileIn
	