comparing
hash
	| hash |

	hash := self species hash.
	1 to: self size do: [:i | hash := (hash + (self at: i) hash) hashMultiply].
	^hash