ParseTreeRewriter walks over and transforms its RBProgramNode (tree). If the tree is modified, then answer is set to true, and the modified tree can be retrieved by the #tree method.

Here is a little script to rewrite a self halt into self dormantHalt. 

	| rewriter node |
	rewriter := RBParseTreeRewriter new.
	rewriter replace: 'self halt' with: 'self dormatHalt'.
	node := (ProtoObjectTest>>#testIfNil) parseTree.
	rewriter executeTree: node.
	^ node formattedCode

Note how do we get the transformed code. 

Have a look at the users of deprecated:

		deprecated: 'Please use #isPinnedInMemory instead'
		transformWith: '`@receiver isPinned' -> '`@receiver isPinnedInMemory'.

You can also have a look at the ParseTreeRewriterTest class


Instance Variables:
	tree	<RBProgramNode>	the parse tree we're transforming
			
	