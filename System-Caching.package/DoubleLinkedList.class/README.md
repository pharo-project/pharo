I am DoubleLinkedList, an ordered list data structure consisting of objects, most likely DoubleLinks or something compatible, connected to each other by forward and backwards links.

Note that some of my API deals with the elements that I hold, like any other collection, while some of my API references the links that I use internally (those usually have the word link in the selector name). Some methods accepts both values or links as argument (like #add:). Because I expose some if my internal structure, I can be broken quite easily.

