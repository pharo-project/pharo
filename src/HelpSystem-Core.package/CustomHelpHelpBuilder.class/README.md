This builder builds help topics from a help topic description (which is typically stored
in a class). 

The help topic description object has to understand the following messages:

  #bookName - should return the name of the help book
  #icon - should return the icon of the help book
  #key - should return a unique key to identify the book
  #pages - should return an array of method selectors to call to get the books pages
