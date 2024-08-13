# The Pharo Refactoring Engine

## Introduction

The Refactoring Engine was originally developed by Don Roberts and John Brant for VisualWorks. It was ported to Squeak and Pharo and saw multiple evolution by multiple contributors.

One goal of this engine was to easily include the code refactoring into the standard development workflow. The refactoring operations help to transform and restructure source code, without to much manual intervention. And without the need to retest every single change.
In addition, some basic primitive refactoring are provided, in a way that more complex operations can be constructed by compositing the primitive ones.

The Tool, or a browser with refactoring support or the whole framework is often just known as the 'Refactoring Browser'. 
That's why all the classes of this framework start with the prefix 'RB'.

### Overview

The following sections give an overview of the refactoring engine, the collaborating classes and components.
We will present an explanation with some examples, how to manually construct and execute refactoring operations, usable for those operation that aren't supported as actions in the default code browser.

A more in-deep description of the components will show that the refactoring engines is not only useful for code refactoring, but it also provides a powerful general purpose search and rewrite engine. This search capability - search for code "patterns" - is used to detect common program errors or just bad code style by the Pharo code critics. 

### Engine architecture

The architecture of the engine is the following: It builds a representation of the program to be refactored (instances of RBClass, RBMethod...), some refactorings complement it with Abstract Syntax Trees.
The refactorings check their preconditions (for applicability validation or behavior preservation) against such program representation.
When the preconditions expressed as RBConditions are true, the refactoring is executed. It does not directly modify Pharo code, but produces a list changes - Such changes can be presented to the developer for validation or modification. On approval the changes actually performs the program changes.

### Core Components

Here is a brief overview of the core components.

- `RBScanner` and `RBParser`. The `RBScanner` and `RBParser` are used by Pharo to create an abstract syntax tree (AST) from the methods source code.
- `ASTProgramNode` and subclasses. These are the base and concrete subclasses for all RB-Nodes representing a syntax node class, like `RBMethodNode`, `RBAssignmentNode`, et cetera.
- `RBParseTreeSearcher` and `RBParseTreeRewriter`. Some refactoring operations use the tree searcher and rewriter for applying a transformation on the abstract syntax tree. They implement a program node visitor.
- `RBClass`, `RBMetaclass`, `RBMethod`. Class and Method meta-models representing a class or method created, removed or modified during a refactoring
operation.
- `RBNamespace`. A namespace is an environment for resolving class and method entities by name and collects all changes resp. changed entities.
- `RBRefactoring` and subclasses. Abstract base classes and its concrete subclasses for refactoring operations. Every basic refactoring operation is implemented as a subclass of the `RBRefactoring` class. A refactoring operation checks the precondition that must be fulfilled and implements the actual code transform method.
- `RBCondition`. Instead of implementing conditions and condition checking code into every single refactoring operation, the RBCondition class implements a set of common tests and can be created and combined to realize a composition of conditions.
- `RBRefactoryChange`. Applying a refactoring within a namespace collects changes without applying the actual change to the system. These changes are represented by `RBRefactoryChange` subinstances and a composition of refactory changes.
