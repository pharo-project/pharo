# Code pattern description

A pattern expression is very similar to an ordinary Pharo expression, but allows one to specify some “wildcards”. 
The purpose is simple. Imagine that you have a piece of code:

```
car isNil ifTrue: [ ^ self ].
```

You can of course compare it with the same piece of code for equality, but wouldn’t it be cool if you could compare something similar, but ignore the fact that the receiver is named car? With pattern rules you can do exactly that. Consider the following code and notice the back-tick before car:

```
`car isNil ifTrue: [ ^ self ].
```

Now this expression can match any other expression where `isNil ifTrue: [^self]` is sent to any variable (or literal). With such a power you can find all the usages of `isNil ifTrue:`. Using the rewrite engine you can even automatically replace them with `ifNil:`. 

The question is then to know the “wildcards” and how they work.
The Pharo rewrite engine originally developed by John Brant and Don Robert proposes the following patterns:
- Basic pattern: `a
- literal patterns: `#
- List patterns: `@
- Statement patterns: `.
- Block patterns: `{}

We detail them.


## (`)Basic pattern nodes

Code prefixed with a back-tick character (`) defines a pattern node. The list below shows some simple patterns that can be declared with the back-tick:


#### Variable	
This pattern will match message asString sent to any receiver, disregarding the name of it.
```
`someName asString
```


#### Message	
This pattern will match any unary message sent to Pharo globals.
```
Pharo globals `someMessage
```

This pattern will match any selector followed by aVal.
Note that if you want to match a selector with multiple keywords such as `between:and:` you will have to use a list pattern (`@) as shown below.
```
`sel: aVal
```

#### Method
This pattern will match any method which returns nil.

```
`someMethod ^ nil
```
 
Note that the matcher should be configured to work on method and not just expressions.
The MatchTool offers a checkbox to indicate this.



#### Example with matches:

```
`receiver foo 
```
matches:

```
self foo
x foo
OrderedCollection foo
```

Note that the pattern will not match the following expression. 
If you want to match such expression you have to use a list pattern (`@) as shown below.
```
self bar foo
```


## (`#) Literal pattern nodes

A back-tick can be followed by the hash sign to ensure that matched receiver will be a literal.

#### Example:

This pattern will match any literal (Number, String, Array of literal ) followed by `asArray` message
```
`#literal asArray	x
```

#### Example:

```
 `#lit size
```
matches:

```
3 size
'foo' size
#(a b c) size
```

## (`@) List pattern nodes

To have complete flexibility, there is the possibility to use an at sign @ before the name of a pattern node.
This turns the node into a list pattern nodes, which can be empty, returns one or multiple values.
It can be apply to different nodes: expression, selectors, temporaries, arguments, or statements. 

### Expression	
This pattern will match a single or multiple entities followed by the message `isPatternVariable`

```
`@expr isPatternVariable
```

### Selector
This pattern will match any selector (including unary)  of message sent to `myVar`
```
myVar `@message
```

### Temporary variable	

This pattern will match at least one temporary variable which is defined as `temp.
For`@temps, the matching can find nil, one or many temporary variables defined.

```
|`temp `@temps|
```

### Arguments
This pattern will match myDict at: 1 put: followed by a list of arguments `@args that can be nil, one or many args.

```
 myDict at: 1 put:`@args
```

### List of statements	
We will explain statements later on, but this is to mention that @ can be used also to define a list of statements which can be empty, contain one or many elements.

```
[ `.@statements.
 `var := `myAttribute. ]
```

This expression will match a block which has at first a list of statements, that must be followed by 1 last assignment statement `var := `myAttribute.

Remarks:

You may write an expression with just args instead of `@args.
The list patterns does not make any sense for literal nodes i.e. `#@literal.

### Example 1:

```
`x := `@value
```

matches:

```
myVar := OrderedCollection new
```

### Example 2:

```
`sel1 at: `@args1 `sel2: `@args2
```

matches:

```
self at: index putLink: (self linkOf: anObject ifAbsent: [anObject asLink])
```

Where:

```
`args1 and `args2 have different values
`sel1 matches self
`@args1 matches index
`sel2: matches putLink:
`@args2 matches (self linkOf: anObject ifAbsent: [anObject asLink])
```

### Example 3:

```
`@rcvr `@msg: `@args matches:
```

matches 

```
(self class deferUpdates: true) ifTrue: [^aBlock value].
```

Where:
```

`@rcvr matches (self class deferUpdates: true)
`@msg: matches ifTrue:
`@args matches [^aBlock value]
```

### Example 4:

```
|`@args1 `myArgument `@args2| matches:
```

```
| t1 t2 |
```

Here we need to have at least 1 argument myArgument , and the example is matching because `@args1 can be empty. So here we have:


- myArgument is matching with t1
- `@args2 is matching with t2



## (`.) Statement pattern nodes

Back-tick can be followed by a period to match statements. For example:

Pattern type	Pattern node	Description
Statement	var
ifTrue: [`.statement1 ]
ifFalse: [ `.statement2 ]	This pattern will match an ifTrue:ifFalse: message send to any variable, where both blocks have only one statement each.

### Example1:

```
`.Statement1.
```

is matching:

```
x := 1.
myVal:= 'Hello World'.
self assert: myVal size equals: 11.
```

### Example2:

```
|`@temps|
`@.statements1.
`.duplicate.
`@.statements2
```
matches:

```
|x|
x := 1.
x := 2
```
Where:

```
|`@temps| matches |x|
`@.statements1. is nil
`.duplicate. matches x := 1.
`@.statements2

```

P.S. In the end it does not matter whether you will write `.@Statement or `@.Statement.

### (`{ }) Block Pattern Nodes

These are the most exotic of all the nodes. They match any AST nodes like a list pattern and test it with a block. 
The syntax is similar to the Pharo block, but curly braces are used instead of square brackets and as always the whole expression begins with a back-tick.

Consider the following example:

```
`{ :node | node isVariable and: [ node isGlobal ] } become: nil
```

this pattern will match a message `#become:` with an attribute nil, where the receiver is a variable and it is a global variable. 

There is also a special case called wrapped block pattern node which has the same syntax and follows a normal pattern node. 
In this case first the node will be matched based on its pattern, and then passed to the block. 

#### Example:

```
`#arr `{ :node | node isLiteralArray } asArray
```

is a simple way to detect expression like #(1 2 3) asArray. In this case first #(1 2 3) will be matched by the node and then tested by the block.

## Naming is Important

The pattern nodes are so that you can match anything in their place. But their naming is also important as the code 
gets mapped to them by name. 

#### Example:

```
`block value: `@expression value: `@expression
```

will match only those `#value:value:` messages that have exactly the same expressions as both arguments. 
It is like that because we used the same pattern variable name.
