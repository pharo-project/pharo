# ASTParseTreeSearcher description

After figuring out what are the patterns that can be used and what kind of matches they can perform, now we can move forward to 
discover how `ASTParseTreeSearcher` class works in Pharo. 
It will help use to understand in the last section how ASTParseTreeSearcher and defined patterns work together to find the matches we are looking for.

`ASTParseTreeSearcher` looks for a defined pattern using the ‘wildcards’ of a matcher defined as a Tree, and on success (when match is found) 
a block is executed.



### The matches:do: message

`#matches:do:` which a message that looks for patterns defined in `matches:` block using the wildcards, and if a match is found the `do:` block is executed.
The do block takes two parameters: `:aNode` and `:answer`. The `aNode` refers to each node of the pattern defined, and the answer can be used for example 
to increment value on each node match.

The blocks defined in `#matches:do:` are called rules, and they are stored only in success in instance searches of ASTParseTreeSearcher defined below.

### The executeTree: message
`executeTree:` takes aParseTree as input parameter, which is the possible matching code that we are looking for, and starts the matching process 
using the defined pattern messages of type OrderedCollection, and returns the list of messages found in a match.

### The hasRules message
`hasRules` returns searches list

### Instance variables

Basically, when a developer uses this class, the most used instance variables are:

- `searches` whose type is an ordered collection, contains all the successful rules applied whenever using: `#matches:do:`, `#matchesMethod:do` … to store 
rules of type `Rule`, `MethodRule`, `ArgumentRule`, `TreeRule`,...
- `context` whose type is a dictionary: contains all the successfully matched patterns.


Consider the following example which is using the methods presented above:

```
|searcher dict|
searcher := ASTParseTreeSearcher new.
searcher
    matches: '`@rcv at:`@arg `sel:`@arg1'
    do: [ :aNode :answer | dict := searcher context ].
searcher executeTree:
    (RBParser parseExpression: 'cache at: each ifAbsentPut: [ each ].').
```

The method `#matches:do:` is used to define the pattern that we are looking for, using the ‘wildcards’ defined in first section.
In addition of that, the `do:` is running only on match, and in our case it will fill the dictionary dict with the searcher context 
(which is the pattern defined in matches block).

This execution is fired on `executeTree:` which defines the matcher that is a String parsed as a Tree using parseExpression, 
then starts matching it with the pattern.
