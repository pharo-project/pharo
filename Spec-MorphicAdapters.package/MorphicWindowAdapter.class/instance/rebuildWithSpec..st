protocol
rebuildWithSpec: aSpec
	| sub |
	
	sub := self model window submorphs copy.
	self model window removeAllMorphs.
	sub allButLast do: [:e | self model window addMorphBack: e ].
	self model addModelIn: self widget withSpec: aSpec