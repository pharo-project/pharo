I am a tool that allows you to reoptimize messages into a decompiled method.

I receive an AST and I replace non-optimized message nodes that have been reconstructed by the AST builder into optimized message nodes, then I return the AST.

Public API : use rewriteAST: to transform it with optimized messages.

example: FBDOptimizedMessagesRewriter rewriteAST: (Object >> #asString ) ast.

I only rewrite nodes with "reconstructed" property to ensure that I reoptimize only the messages that were optimized in the non-decompiled method.