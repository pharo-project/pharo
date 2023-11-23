"
HeuristicCompletion is a code completion engine using generators and heuristics. Instead of query the entire system and generate lots of potentially intermediate collections that we need to concatenate, copy, grow, iterate all those sub-collections in order. Generators provide a stream-like access to those collections (and sequences of collections) without scanning the full system eagerly.

# Architecture
This completion engine is done out of three main components:

- lazy fetchers implemented using generators (cf. `CoFetcher`),
- a completion object that works as a result-set (cf. `CoResultSet`),
- and several completion heuristics that are decided depending on the code being completed by looking at the AST (cf. `CoASTNodeFetcherHeuristic`).

# Plugging different heuristics

This completion engine is meant to be pluggable. New heuristics can be introduced, or the ones in the system can be completely replaced. If you want to implement your own completion you need to subclass CoASTResultSetBuilder or CoResultSetBuilder. CoASTResultSetBuilder provides already common behavior when basic the code completion algorithm on the AST. Then the completion engine can be configured with the required result set builder.

```
CoCompletionEngine new
    complishonBuilder: MyResultSetBuilder new;
    yourself
```

## Subclassing CoResultSetBuilder
The system will provide your builder with a completion context, and will then call `buildComplishon`. You need to redefine `buildComplishon` and return a `CoResultSet`.

## Subclassing CoASTResultSetBuilder
The system will provide your builder with a completion context, and will then call a default version of `buildComplishon`. By default it will parse the source code to get an AST, get the AST node corresponding to the caret position, and make a double dispatch on the node. As a result, the `ASTResultSetBuilder` will be sent a corresponding `visit*` with the corresponding node. You need to redefine `visit*` and return a `CoResultSet` configured depending on the AST node.

## Subclassing CoASTHeuristicsResultSetBuilder
`CoASTHeuristicsResultSetBuilder` is an `CoASTResultSetBuilder` based on heuristics. It has three sets of heuristics: one for messages, one for variables, and one for methods. You can redefine `messageHeuristic`, `methodHeuristic` or `variablesHeuristic` to change one of them.
"
Class {
	#name : 'ManifestHeuristicCompletionModel',
	#superclass : 'PackageManifest',
	#category : 'HeuristicCompletion-Model-Manifest',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestHeuristicCompletionModel class >> ruleReSendsDifferentSuperRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#'CoSystemEnvironment class' #forSmalltalkGlobals #true)) #'2020-03-26T14:29:31.095031+01:00') )
]

{ #category : 'code-critics' }
ManifestHeuristicCompletionModel class >> ruleReTempsReadBeforeWrittenRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#CoFetcher #next: #false)) #'2020-03-26T14:50:53.739628+01:00') #(#(#RGMethodDefinition #(#CoTypeInferencer #visitCascadeNode: #false)) #'2020-03-26T14:50:59.173856+01:00') )
]

{ #category : 'code-critics' }
ManifestHeuristicCompletionModel class >> ruleReUsesTrueRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#CoTypeInferencer #visitMessageNode:receiverType: #false)) #'2020-03-26T14:21:20.676631+01:00') )
]
