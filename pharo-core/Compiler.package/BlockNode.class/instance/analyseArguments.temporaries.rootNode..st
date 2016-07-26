code generation (closures)
analyseArguments: methodArguments temporaries: methodTemporaries rootNode: rootNode "<MethodNode>" "^<Sequence of: <TempVarNade>>"
	"Top level entry-point for analysing temps within the hierarchy of blocks in the receiver's method.
	 Answer the (possibly modified) sequence of temp vars.
	 Need to hoist temps out of macro-optimized blocks into their actual blocks.
	 Need to note reads and writes to temps from blocks other than their actual blocks to determine
	 whether blocks can be local (simple slots within a block/method context) or remote (slots in
	 indirection vectors that are shared between contexts by sharing indirection vectors).

	 The algorithm is based on numbering temporary reads and writes and block extents.
	 The index used for numbering starts at zero and is incremented on every block entry
	 and block exit.  So the following
		| a b blk r1 r2 t |
		a := 1. b := 2. t := 0.
		blk := [ | s | s := a + b. t := t + s].
		r1 := blk value.
		b := -100.
		r2 := blk value.
		r1 -> r2 -> t
	is numbered as
		method block 0 to: 6:
		| a b blk r1 r2 t |
		a w@1 := 1. b w@1 := 2. t w@1 := 0.
		blk w@5 := [entry@2 | s |
					 t  w@3 := t r@3 + a r@3 + b r@3
					] exit@4.
		r1 w@5 := blk r@5 value.
		b w@5 := nil.
		r2 w@5 := blk r@5 value.
		r1 r@5 -> r2 r@5 -> t r@5
	So:
		b and blk cannot be copied because for both there exists a write @5 that follows a
			read @4 within block 2 through 4
		t must be remote because there exists a write @3 within block (2 to: 4)
	Complications are introduced by optimized blocks.  In the following temp is written to
	after it is closed over by [ temp ] since the inlined block is executed more than once.
		| temp coll |
		coll := OrderedCollection new.
		1 to: 5 do: [ :index | 
			temp := index. 
			coll add: [ temp ] ].
		[(coll collect: [:ea| ea value]) asArray = #(5 5 5 5 5)] assert.
	In the following i is local to the block and must be initialized each time around the loop
	but if the block is inlined it must be declared at method level.
		| col |
		col := OrderedCollection new.
		1 to: 3 do: [ :each | | i | i := each. col add: [ i ]. i := i + 1 ].
		[(col collect: [ :each | each value ]) asArray = #(2 3 4)] assert"
	[arguments isEmpty or: [arguments hasEqualElements: methodArguments]] assert.
	arguments := methodArguments asArray. "won't change"
	[temporaries isNil or: [temporaries isEmpty or: [temporaries hasEqualElements: methodTemporaries]]] assert.
	temporaries := OrderedCollection withAll: methodTemporaries.

	[optimized not] assert. "the top-level block should not be optimized."
	self analyseTempsWithin: self rootNode: rootNode assignmentPools: Dictionary new.

	"The top-level block needs to reindex temporaries since analysis may have rearranged them.
	 This happens when temps are made remote and/or a remote node is added."
	temporaries withIndexDo:
		[:temp :offsetPlusOne| temp index: arguments size + offsetPlusOne - 1].

	"Answer the (possibly modified) sequence of temps."
	^temporaries asArray