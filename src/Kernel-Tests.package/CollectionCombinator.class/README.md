For a collection of collections, enumerate all elements of the cartesian product. The code shows how recursion is used to implement variable nesting of loops.
The cartesian product is usually a huge collection, that should not be kept in memory. Therefore the user of the class has to provide a block with one argument that is called each time a tuple is constructed. When possible, that block should not build a collection of all these tuples, but should immediately drop unsuitable tuples. 
To get a first impression, try this with 'inspect it':

     | result |
     result := OrderedCollection new.
    CollectionCombinator new
         forArrays:  (OrderedCollection with: #(#a #b #c)
                                             with: #(1 2 3 4 5)
                                             with: #('v' 'w' 'x' 'y' 'z')
                                             with: #('one' 'two' 'three')
                         )
         processWith: [:item |result addLast: item].
    result
         