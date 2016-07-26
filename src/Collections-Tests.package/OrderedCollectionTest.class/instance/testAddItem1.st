tests - adding
testAddItem1

   | collection size |
   collection := #('Jim' 'Mary' 'John' 'Andrew' ) asOrderedCollection.
   size := collection size.
   collection add: 'James' before: 'Jim'.
   collection add: 'Margaret' before: 'Andrew'.
   self assert: size + 2 = collection size.
