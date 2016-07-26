A Categorizer is responsible to manage the class categories and method protocols. 

Instances consist of an Array of category names (categoryArray), each of which refers to an Array of elements (elementArray). This association is made through an Array of stop indices (categoryStops), each of which is 
the index in elementArray of the last element (if any) of the corresponding category. For example: categories := Array with: 'firstCat' with: 'secondCat' with: 'thirdCat'. stops := Array with: 1 with: 4 with: 4. 
elements := Array with: #a with: #b with: #c with: #d. This means that category firstCat has only #a, secondCat has #b, #c, and #d, and thirdCat has no elements. This means that stops at: stops size must be the same as elements size.

 Instance Variables
      categoryArray:          <SequenceableCollection of: Object>
      categoryStops:          <SequenceableCollection of: Integer>
      elementArray:           <SequenceableCollection of: Object>

categoryArray
       - holds the list of categories.
       A category could be any Object but is generally a String or Symbol.
       Categories should be unique (categoryArray asSet size = categoryArray size)

categoryStops
       - holds the index of last element belonging to each category.
       There should be a category stop for each category (categoryStops size = categoryArray size).
       The categoryStops should be sorted (categoryStops sorted = categoryStops).
       A category stop equal to its predecessor (= 0 for the first category stop) denotes an empty category.

elementArray
      - holds the elements to be classified. The elements are sorted by category.

Class variables
       Default is the default category used to classify yet unclassified methods of a class
       NullCategory is the category to be displayed in a Browser for a class having no method.