elementary steps
diffPackages
	"return a list of packages that are new. They can be dirty or not."
	"self new diffPackages"
	 
	| diff |
	diff := Set new.
	self currentVersionsToBeSaved do: [:each |
		(PackagesBeforeLastLoad includes: each) ifFalse: [diff add: each]].
	^ diff
	