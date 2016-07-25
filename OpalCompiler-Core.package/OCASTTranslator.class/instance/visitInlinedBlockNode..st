visitor-double dispatching
visitInlinedBlockNode: anOptimizedBlockNode

	"We are visiting a scope that is not a block, but inlined in the outer context.
	This means: 
			- we do not create a Block
			- we call IRBuilder to add temps
	"
	
	| argumentNames tempNames  copiedNames  |
	argumentNames := anOptimizedBlockNode argumentNames asArray.
	tempNames := (anOptimizedBlockNode scope tempVars collect: [ :each | each name asSymbol]) copyWithoutAll: argumentNames.
	copiedNames := (anOptimizedBlockNode scope inComingCopiedVars collect: [:each | each name]) asArray.
	
	
	anOptimizedBlockNode scope tempVector ifNotEmpty: [
		methodBuilder 
			createTempVectorNamed: anOptimizedBlockNode scope tempVectorName 
			withVars: (anOptimizedBlockNode scope tempVector collect: [:each| each name]) asArray.
	].
	
	methodBuilder addTemps: tempNames.
	methodBuilder addTemps: copiedNames.
	methodBuilder addTemps: argumentNames.
	anOptimizedBlockNode isInlinedLoop ifTrue: [
		tempNames do: [ :tempName |
			methodBuilder pushLiteral: nil.
			methodBuilder storeTemp: tempName.
			methodBuilder popTop.
		 ]].
	
	self visitNode: anOptimizedBlockNode body.