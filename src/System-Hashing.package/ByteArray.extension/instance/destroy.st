*System-Hashing-Core
destroy
	1 to: self size do: 
		[ : x |
		self at: x put: 0 ]