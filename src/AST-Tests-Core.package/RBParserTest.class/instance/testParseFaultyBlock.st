tests parsing
testParseFaultyBlock
	| node blockNoArgButBar blockNoArgButColon unfinishedBlock |
	"parsing block should mark this block as faulty, if it is unfinished no closing brace, or missing parts (colon but no arg, 
	no arg but vertical bar)"
	blockNoArgButBar := '[ | ]'.
	blockNoArgButColon := '[ : ]'.
	unfinishedBlock := '[  '.
	{blockNoArgButBar.
	blockNoArgButColon.
	unfinishedBlock}
		do: [ :faultyExpression | 
			node := self parseFaultyExpression: faultyExpression.
			self assert: node isBlock.
			self assert: node isFaulty]