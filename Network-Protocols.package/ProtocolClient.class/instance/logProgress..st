private
logProgress: aString
	self progressObservers do: [:each | each show: aString].
