*Manifest-Core
arrayStringForManifest
	| array |
	array := Array new:2.
	array at: 1 put:  (self class asString asSymbol).
	array at: 2 put: {self parentName asSymbol.  self selector asSymbol. self isMetaSide asString  asSymbol}.
	^ array 
	 