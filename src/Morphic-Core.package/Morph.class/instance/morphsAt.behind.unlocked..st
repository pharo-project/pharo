submorphs-accessing
morphsAt: aPoint behind: aMorph unlocked: aBool 
	"Return all morphs at aPoint that are behind frontMorph; if aBool is true return only unlocked, visible morphs."

	| isBack all tfm |
	all := (aMorph isNil or: [owner isNil]) 
				ifTrue: 
					["Traverse down"

					(self fullBounds containsPoint: aPoint) ifFalse: [^#()].
					(aBool and: [self isLocked or: [self visible not]]) ifTrue: [^#()].
					nil]
				ifFalse: 
					["Traverse up"

					tfm := self transformedFrom: owner.
					all := owner 
								morphsAt: (tfm localPointToGlobal: aPoint)
								behind: self
								unlocked: aBool.
					WriteStream with: all].
	isBack := aMorph isNil.
	self submorphsDo: 
			[:m | | found | 
			isBack 
				ifTrue: 
					[tfm := m transformedFrom: self.
					found := m 
								morphsAt: (tfm globalPointToLocal: aPoint)
								behind: nil
								unlocked: aBool.
					found notEmpty 
						ifTrue: 
							[all ifNil: [all := Array new  writeStream].
							all nextPutAll: found]].
			m == aMorph ifTrue: [isBack := true]].
	(isBack and: [self containsPoint: aPoint]) 
		ifTrue: 
			[all ifNil: [^Array with: self].
			all nextPut: self].
	^all ifNil: [#()] ifNotNil: [all contents]