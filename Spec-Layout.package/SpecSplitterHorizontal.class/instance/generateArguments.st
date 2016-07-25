protocol
generateArguments
	| index |

	index := self commands identityIndexOf: self.

	self arguments: { index }