comparing
hash

	| hash |

	hash := self method hash bitXor: self keyword hash.
	1 to: self basicSize do: [:index | hash := hash bitXor: (self basicAt: index) hash].

	^hash.