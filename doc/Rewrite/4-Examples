3. ASTParseTreeSearcher examples with pattern code

Finally, in this section we use patterns with the ASTParseTreeSearcher class and do some magic by finding some matches in Pharo code !

Consider the following example:

```
| dict searcher|
searcher := ASTParseTreeSearcher new.

searcher  
   matches: '`@receiver assert: `@arg equals: true'
   do: [ :aNode :answer | dict := searcher context ].

searcher 
   executeTree: (RBParser parseExpression: 'self assert: reader storedSettings first realValue equals: true.').

dict 	
   collect: [ :each | each displayString ].
```

The example is matching successfully and the dictionary dict will return different values during the iteration:

- Match 1: (key) `@receiver is matching with (value) self
- Match 2: (key) `@arg is matching with (value) reader storedSettings first realValue

If we want to check all the messages in the matcher, we can use searcher messages which will return an array of one item containing message 
`#assert:equals:` as it is the only message available in the matched expression.
